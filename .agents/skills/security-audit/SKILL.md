---
name: security-audit
description: Audit mutsu for security problems from the point of view of an interpreter security engineer - trust boundaries (analysis must not execute, module search paths, caches), memory safety of unsafe/GC/NaN-boxing/threads, bounded recursion and allocation on untrusted input, files and fds the runtime creates, the playground, CI/release secrets and agent prompt injection. Use for a periodic audit, before merging a change that touches one of those surfaces, or when asked "is this safe / any security concerns".
metadata:
  short-description: Threat-model-driven security audit of mutsu
---

# Security audit

The threat model and the invariants live in [docs/security.md](../../../docs/security.md); read
it first. This skill is the procedure. It is written for mutsu, not as a generic checklist:
**running a Raku script is trusted** (it can `run` anything), so an audit is about the surfaces
where *less* than "run this program" happens and must not become it, and about memory safety,
which a trusted script must not be able to break either.

## 0. Scope the audit

- **Diff audit** (a PR or branch): `git diff --name-only origin/main...` and map each file to the
  surfaces table in `docs/security.md`. Audit only the surfaces touched, plus their callers.
- **Full audit**: one pass per surface below. On the 12-core box, fan out read-only sub-agents
  (one per section 2-6; they build nothing); on a ~4-core remote container do them inline or with
  at most one building agent. Give each agent the section text, `docs/security.md`, and the
  instruction to write proofs of concept under `tmp/sec-audit-<area>/` only.

## 1. Tools first (cheap, mechanical)

```sh
cargo audit                                   # RustSec; cargo install --locked cargo-audit
zizmor --offline .github/workflows            # pip install zizmor
cargo clippy -- -W clippy::undocumented_unsafe_blocks 2>&1 | grep -c undocumented_unsafe
rg -n 'unsafe impl (Send|Sync)' src crates    # every hit needs a justification
checksec --file target/release/mutsu          # if checksec is installed: PIE/RELRO/NX/canary
```

Long or nightly runs (sanitizers, Miri, fuzzing — commands in `docs/security.md`) go through
`scripts/dev run <name> -- ...` and are opt-in for a full audit, not for a diff audit.

## 2. Analysis must not execute

- Grep for every path that builds an `Interpreter` or runs a module at parse time:
  `rg -n 'Interpreter::new|parse_time_exports|slang_activation' src/parser src/runtime src/analysis`
  and check each is gated off for `src/analysis/`, `crates/mutsu-lsp` and `--dump-*`.
- PoC shape: a `lib/Evil.rakumod` whose mainline writes a marker file and whose `EXPORT` is
  computed (or that defines a slang), and a `victim.raku` with `use lib 'lib'; use Evil;`. Then
  `timeout 30 ./target/debug/mutsu --dump-ast victim.raku` must not create the marker.
- Also try `BEGIN`, `constant` initializers, traits, regex code blocks and `is export` subs.

## 3. Code loading, caches, files, processes

- Read `src/runtime/run_modules.rs` for every candidate directory. Anything not named by
  `use lib` / `-I` / `MUTSULIB` / the installed repo / bundled batteries is a hijack vector; prove
  it with a planted module in an ancestor directory of the script.
- `src/precomp.rs`: cache key (path, hash, binary identity), location, permissions, decode limits.
- Files the runtime creates by itself: `rg -n 'OpenOptions|create_dir_all|File::create|libc::open' src`
  outside the IO builtins. Check path origin (cwd-relative? `/tmp`?), `O_NOFOLLOW`, `O_EXCL`, mode,
  and whether the content holds argv / source / env. `src/crash_report/` is the known example.
- fds: `rg -n 'libc::pipe\b|libc::socket|libc::open' src` — each needs `O_CLOEXEC`. PoC: in a
  child, `ls -l /proc/self/fd` after `signal(SIGTERM).tap({...})` and a `:merge` run.
- `rg -n 'Command::new\("(kill|sh|env)' src`: helpers resolved through `$PATH`.
- Environment variables (`rg -n 'env::var\("MUTSU' src`): which ones redirect a write or a code
  load. They are trusted like `LD_PRELOAD`, but none may enable execution in an analysis path.

## 4. Memory safety

- Inventory: `rg -n 'unsafe' src crates | wc -l`, grouped by directory; list blocks without a
  `SAFETY:` comment in the files you touch.
- Threads are the main attack surface. For each `&self -> &mut T` primitive (`gc_contents_mut`,
  `SyncUnsafeCell::get`, native-array caches), ask: can two Raku threads reach the same object?
  Parameters (`sub f(@x)`), attributes and nested containers bypass the name-keyed lanes. PoC
  shape: 8 `start` blocks mutating one `%h` or `@a` passed as a parameter; run it 3-5 times on a
  debug **and** a release build. Exit 139/134 or a nanbox assertion is a confirmed bug.
- Raw pointers: `rg -n 'as \*mut|as \*const|from_raw_parts|from_utf8_unchecked|transmute' src`.
  For each, find where the pointee's lifetime ends (guard drop, `Vec` push/reallocation, frame
  pop, thread-local re-pointing).
- JIT (`src/vm/vm_jit*.rs`): guards checked before inline code, bounds checks in helper shims,
  W^X (no RWX mapping), transmuted fn pointers only from published entries. Compare results with
  `MUTSU_JIT=on` and off on boundary values (48-bit ints, mixed types).
- NativeCall is FFI and may do anything; flag only unsafety reachable *without* `is native`.

## 5. Bounded resources on untrusted input

Try each with `timeout 30` (and `ulimit -v 4000000` for allocation cases); a *catchable* error is
the pass condition, SIGSEGV/SIGABRT/OOM-kill is a finding:

```raku
EVAL('(' x 5000 ~ '1' ~ ')' x 5000)                       # parser depth
Rakudo::Internals::JSON.from-json('[' x 1_000_000 ~ ']' x 1_000_000)
my $a = 1; $a = 1 => $a for ^100_000; say $a.Str.chars     # printer depth
my $b = []; $b = [$b] for ^1_000_000; $b = Nil             # Drop depth
sprintf('%9999999999d', 1); 'abc'.indent(99999999999); 1 +< 9999999999
Rakudo::Internals::JSON.from-json(q{"\u000é"})            # str slicing panic
```

Wrap each in `try { ... }; say "survived"`. Also check the wasm build's assumptions
(`src/vm/vm_stack_guard.rs`: the guard is off on wasm; panics abort there).

## 6. Playground, CI, release, agents

- `site/`: output inserted with `textContent`, never `innerHTML`; URL fragments load code but
  never auto-run it; `autorun` only for author-written embeds; long runs off the main thread.
- `.github/workflows`: run zizmor; then by hand: which secrets each job sees and whether they sit in
  an `environment:`; `pull_request_target` / `workflow_run` / `issue_comment` triggers; `${{ }}` in
  `run:`; who can push a `v*` tag; whether `scripts/ci-docs-only.sh` lets an executable path skip
  the build.
- Agent surface: any skill or script that copies issue/PR/comment text or dist test output into
  an agent prompt or a committed file; claim/lock comments accepted from non-members.

## 7. Report and file

- Rank each finding Critical / High / Medium / Low / Info with `file:line`, the invariant it
  breaks (cite the `docs/security.md` heading), the attacker scenario, and **confirmed (PoC) vs.
  suspected**. "By design: a script can do anything" is a valid verdict — say which boundary
  makes it so.
- Before filing, re-run each PoC once yourself; never report a sub-agent's claim unverified.
- Public disclosure is the maintainer's call: report findings to the user first, then file one
  `tokuhirom/mutsu` issue per finding (`todo:ticket` / `todo:deep`) once they agree. Do not put
  exploit-ready recipes for an unfixed High/Critical into a public issue or doc without that
  agreement.
- A fix carries a regression test (the PoC, reduced) in the matching `t/` category; a fix that
  restores an invariant updates `docs/security.md` if the invariant's wording changes.
- Delete `tmp/sec-audit-*` and any `tmp/crash/` reports your PoCs produced when done.
