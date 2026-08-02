# `$*KERNEL` / `$*DISTRO` should be built lazily and cached, not eagerly at every startup

Found 2026-08-02 while adding the Miri gate (ADR-0013 §4 phase 4): the gate's interpreter-level
smoke tests could not run because `Interpreter::new()` spawns processes.

## What happens today

`Interpreter::new()` → `init_io_environment` (`src/runtime/io_env.rs:86`) eagerly constructs
`$*DISTRO`, `$*PERL`, `$*RAKU`, `$*VM` and `$*KERNEL` and hoists them into the process-global env
base tier (`IMMUTABLE_BASE_DYNAMICS`, `src/runtime/mod.rs`). Constructing them **shells out**:

| site | spawns |
| --- | --- |
| `make_distro_instance` (linux arm), `src/runtime/io_sysinfo.rs:154` | `uname -r` |
| `make_kernel_instance`, `io_sysinfo.rs:428` | `uname -r` **again**, via a second `OnceLock` |
| `make_kernel_instance`, `io_sysinfo.rs:440` | `uname -m` |
| `make_kernel_instance`, `io_sysinfo.rs:470` | `hostname` |
| `make_distro_instance` (macos arm), `io_sysinfo.rs:116/122/128` | `sw_vers` ×3 |

So every `mutsu` invocation — including every `prove` test process and every `mzef` subprocess —
pays three `fork`/`exec`s on Linux (four on macOS) for data that most programs never read. The
project's headline metric is startup time (0.04× raku), which makes this the wrong place to be
eager. The values are process constants, so they should be **delayed until first access and then
cached**, which is what Rakudo effectively does.

Note also the duplication: kernel release is fetched twice through two separate `OnceLock`s
(`DISTRO_UNAME_R` and `UNAME_R`), which is a "1 operation = 1 implementation" violation on its own.

## Why it blocks the Miri gate

Miri cannot spawn a process (`unsupported operation: can't call foreign function
`posix_spawnattr_init``), so `Interpreter::new()` aborts under Miri before reaching any container
code. That is why `src/gc/soundness_smoke.rs`'s four tests — the ones that would let Miri watch the
VM take an aliased `&mut` into a shared container node — are `#[cfg_attr(miri, ignore)]` today, and
the gate currently covers the `Gc` primitive only. ADR-0013 §4 explicitly warned that a
primitive-only Miri run "mostly re-proves std's `UnsafeCell` guarantee", so closing this ticket is
what gives the gate its full value.

(A second, smaller Miri blocker sits in `local_timezone_offset_secs()`, `src/runtime/mod.rs`: it
calls `libc::time` / `libc::localtime_r`, which Miri also cannot call. Its existing contract already
says "returns 0 if the offset cannot be determined", so extending that arm with `not(miri)` is a
one-line fix to do at the same time.)

## Two slices, in order

1. **Cheap and independent: stop shelling out at all.** Replace the `uname -r` / `uname -m` pair
   with one `libc::uname(2)` call (one syscall, no fork) behind a single cached helper used by both
   `make_distro_instance` and `make_kernel_instance`, and replace `hostname` with
   `libc::gethostname`. This fixes the duplication and removes the fork/exec from startup even
   before laziness lands. Keep the current shell-outs as the fallback for platforms without the
   syscall, and give the `miri`/`wasm32` arm `/proc/sys/kernel/osrelease` +
   `std::env::consts::ARCH` (verified equal to `uname -r` on Linux).
2. **The real fix: delay construction.** Do not build these instances in `init_io_environment`;
   materialize on first read and cache. The awkward part is *where* the hook goes: these live in the
   process-global base tier (`crate::env::set_global_base`, a `OnceLock` set once at startup), and
   env lookup falls overlay → parent chain → base tier with no miss hook. Either give the base tier
   a lazy-materialization entry point, or leave the names out of it and let the dynamic-var read
   path (`get_dynamic_handle` and the VM's dynamic read) construct-and-insert on demand. Whichever
   is chosen, the cache must stay process-wide so repeated reads do not re-run the syscalls, and it
   must be safe under the threaded lanes that share the base tier.

## Pins

`roast/S02-magicals/KERNEL.t` and `DISTRO.t` require every field to be truthy, and
`roast/S32-io/IO-Socket-INET.t:337` matches `$*KERNEL.release` against `/Microsoft/` for WSL
detection — so the data must stay correct, not just cheap. `t/base-tier-magic-vars.t` pins the
base-tier hoisting itself.
