# `$*DISTRO`/`$*PERL`/`$*RAKU`/`$*VM`/`$*KERNEL` now materialize on first read, not at every startup

Slice 2 of `todo/tickets/magic-vars-should-be-built-lazily.md` (slice 1, the syscall reduction, landed
2026-08-03). `Interpreter::new()` used to eagerly build all five of these `Instance` values — five
`Value::make_instance` calls, several `Version` parses, a 32-element POSIX signal array, and the `VM`
class's `vm_config` hash — and hoist them into the process-global env base tier
(`IMMUTABLE_BASE_DYNAMICS`) on every process start, even for a program that never reads any of them.
They now materialize on first read and are cached process-wide thereafter.

## Which cost actually dominated

The ticket flagged this as unmeasured and asked to profile before choosing a design. On Linux (this
workspace has no macOS host to verify the `sw_vers` row), `strace -c -f` on `mutsu -e 'say 1'` showed
the syscall side was already negligible after slice 1: one `uname(2)`, ~0.5% of total syscall time.
`perf stat -e instructions -r 30` on the same command showed **construction, not syscalls, was the
real eager cost**: ~7.1M instructions retired before this change vs. ~6.3M after — an ~11% reduction
for a program that touches none of the five vars — while the syscall count and total syscall time
were statistically unchanged between the two binaries (both still show exactly one `uname`, called
once no matter how many of the five vars are read, since the existing per-var `OnceLock` caching
already deduplicated the syscall — slice 1's fix). This matches the ticket's plausible-but-unverified
guess.

## Design chosen

The ticket presented two options: give the base tier itself a lazy-materialization hook, or leave the
five names out of the base tier and let the general dynamic-var read path construct-and-insert on a
miss. The base tier (`crate::env::GLOBAL_BASE`, `src/env.rs`) is a `OnceLock<SymMap>` — write-once for
the whole process — so giving it a genuine miss hook would mean changing its underlying type to
something mutable-after-first-set (e.g. an `RwLock`), which is a much larger, concurrency-relevant
change for five names. The second option turned out to be the smaller, safer one:

- The five vars were removed from `IMMUTABLE_BASE_DYNAMICS` (`src/runtime/mod.rs`) and from the eager
  `self.env.insert(...)` calls in `Interpreter::init_io_environment_impl` (`src/runtime/io_env.rs`).
- A new `Interpreter::lazy_magic_dynamic_var(name)` matches the exact env-key spellings the compiler
  already emits for each var (`"*DISTRO"`/`"?DISTRO"`, `"*PERL"`/`"?PERL"`, `"*RAKU"`/`"?RAKU"`,
  `"$*VM"`/`"*VM"`/`"?VM"`, `"*KERNEL"`/`"?KERNEL"`) and dispatches to the existing per-var
  `cached_*_instance()` `OnceLock`s (unchanged from before — construction is still only paid once per
  process, just on first *read* instead of at `Interpreter::new()`).
- The hook is called from `Interpreter::get_env_with_main_alias_inner` (`src/vm/vm_env_helpers.rs`)
  right after the plain `self.env().get(name)` miss — the chokepoint every dynamic-var read path
  (the VM's `GetGlobal` fast path, `get_dynamic_handle`, ...) already falls through to on a genuine
  miss. It had to go there rather than at the very end of the function: several later fallback
  branches (`twigil_dynamic_alias`, `main_unqualified_name`, ...) `return` unconditionally the moment
  they compute a candidate alias, even when that alias also misses, so a check placed after them was
  unreachable for these names — this was caught by the regression test failing during development,
  not by inspection.

`$*RAKU`/`$*PERL`'s "version" attribute previously came from a hardcoded `Version.new(6)` that was
always immediately overwritten by `update_raku_version_from_parser` (called once per parse, before
execution begins) — since that mutation only fires for an already-materialized instance, and these are
no longer eagerly materialized, `make_perl_instance` now reads `current_language_version()` directly
at construction time via a new shared `language_version_value()` helper, so a lazily-built instance
already reflects the compile unit's `use v6.x` on its first (and only) build.

## No change in values, only in timing

`roast/S02-magicals/KERNEL.t`, `DISTRO.t`, `PERL.t`, `RAKU.t`, `t/base-tier-magic-vars.t`, and
`io_sysinfo_host`'s unit tests all still pass unmodified. A new `t/magic-vars-lazy-materialize.t` pins
object-identity stability across repeated reads (the process-wide cache must materialize once, not per
read), cross-thread visibility of the same cached instance, and `$*PERL !=== $*RAKU` distinctness. A
new Rust unit test, `make_perl_instance_version_reflects_current_language_version` in
`src/runtime/io_sysinfo.rs`, pins the `use v6.x` reflection directly against `make_perl_instance`
(an equivalent `t/`-resident `is_run`-based check tripped an unrelated, pre-existing fragility —
filed separately as `todo/deep/is-run-after-raku-read-swallows-child-spawn.md` rather than blocking
this change, since it does not affect the roast whitelist or the `t/` suite as it stands today).

`src/runtime/io_sysinfo.rs` was split (it was drifting over the 500-line convention): the ~100-line
`make_kernel_instance` moved to a new `src/runtime/io_sysinfo_kernel.rs`.
