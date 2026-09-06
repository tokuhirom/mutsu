# Every shipped configuration is warning-free again, and CI keeps it that way

A warning only exists in the configuration you actually compile. CI compiled
one: the default feature set, on the host, library only (`cargo clippy -- -D
warnings`). Everything outside that one cell was linted by nobody, and by
2026-09-06 three separate blind spots had accumulated 56 warnings between them
— none of them visible on any check, on any PR.

## What was hiding where

**Test code (9 warnings).** The `test` job's Clippy step had no
`--all-targets`, so every `#[cfg(test)]` module in `src/`, the `tests/`
integration targets and the `mzef` bin went unlinted. What had collected there:
a test module with items declared after it (`parser/helpers.rs`), a collapsible
`if let` chain, three `Default::default()`-then-assign patterns, a manual range
check, and three `vec![]`s that wanted to be arrays. The `mutsu-lsp` step had
passed `--all-targets` since it was written, which is exactly why the same rot
never appeared there.

**`jit` off (12 warnings).** The feature set the Miri job builds, and the one
the release matrix falls back to on any target Cranelift cannot build. The JIT
status constants, `JitCodeState`/`JitRangeState`, `CompiledCode::jit`,
`Interpreter::jit_error`, the three `vm_stats` JIT recorders and
`MetaAssignIdentity`'s `u32` conversion pair were all compiled unconditionally
but read only under `#[cfg(feature = "jit")]`. They are gated now, so the
non-JIT interpreter no longer carries a hotness counter and an error slot it
can never use.

**wasm32 (35 warnings).** The configuration the published npm package is built
from (`scripts/build-npm-package.sh`), where the `#[cfg(unix)]` filesystem
paths, the worker pool and PCRE2 all compile out. Three shapes of rot:

- Platform gates placed *inside* a function body rather than at its head, so
  `chmod` and `symlink` (in both `builtins_io_*` and `native_io/io_path_*`)
  computed a mode, resolved paths and built a link target on a platform that
  then refused the operation two statements later — dead work the compiler
  reported as unused locals and unreachable statements. The gate now sits at
  the top of each body: the platform that cannot do the work does not compile
  the work.
- A wasm-only `return state;` in `interval_timer`'s scheduler bootstrap that
  made the shared tail unreachable, when simply not spawning the driver thread
  says the same thing.
- Dead helpers reachable only from a compiled-out caller: `stw_aware_wait`
  (wasm pumps the cooperative scheduler instead of parking),
  `io_lock_failure` (`fcntl` record locks are unix-only), the
  `NativeCall::CStr` address lookup (libffi-only), and the whole Perl 5 pattern
  rewriting module (`:P5` is PCRE2-backed).

`%*ENV`'s OS sweep also moved into an `os_env_hash()` helper with a wasm
counterpart, replacing a `let mut` that nothing mutated in the browser build.

## What the new wasm clippy pass then found

Turning the wasm lint on is not the same as compiling wasm: five clippy
findings existed on that target that `cargo check` never reported, because
nothing had ever run clippy against `wasm32-unknown-unknown`. Four are the
platform-gate shape itself — a `#[cfg(...)] { ... }` block pair reads as a
needless `return` on whichever side ends up last — and they are gone now that
each gate's refusal is the block's value rather than an early return, which
`try_stop_the_world` in the collector had been doing since it was written. The
fifth is real: the Supply tap's receive loop polls `recv_timeout` every 250 ms
natively, but on wasm it breaks out on its first iteration, so it was a loop
that never loops. It is now an `if`/`else` on the wasm side and the polling
loop only where there is something to poll — same behaviour, and the close
guarantee `t/supply-tap-close-interval.t` pins still holds.

Two smaller ones came with them: the crash-report alt-stack no-op returns
`Option<()>` so the caller's guard binding is not a unit `let` on a platform
with no signal handling, and `syscall(0)`'s pid fallback moved from a closure
with a `#[cfg]` body into a `current_pid()` helper with one definition per
platform.

## Keeping it closed

Three changes, so the next warning is caught by a check rather than by a
session that happened to compile something unusual:

- The `test` job's Clippy step is `cargo clippy --all-targets -- -D warnings`
  (job timeout 25 → 30 min; a measured run had four to spare).
- A new `lint-configs` job runs clippy over the two non-default configurations
  — `--no-default-features --features native --all-targets`, and the wasm32
  `--lib` — in parallel with the heavy jobs, so it costs no critical path.
  **It should be added to the repository's required status checks**; the
  existing required set is `test` / `wasm-e2e` / `gc-stress`.
- `make lint` runs all three locally, and the lefthook pre-commit hook grew
  `--all-targets` so it stops giving false assurance about test code.

## Still open

`cargo doc` is a fourth unlinted surface: 210 rustdoc warnings, almost all of
them Raku syntax in prose that rustdoc reads as markup (`[&func]` as a link,
`<key>` as an HTML tag) plus intra-doc links to associated items written
without a `Self::` qualifier. None of it affects the build, and no CI job runs
rustdoc. Filed separately as `todo/tickets/rustdoc-doc-link-warnings.md`.
