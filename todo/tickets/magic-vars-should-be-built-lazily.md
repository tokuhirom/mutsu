# `$*KERNEL` / `$*DISTRO` should be built lazily and cached, not eagerly at every startup

Found 2026-08-02 while adding the Miri gate (ADR-0013 §4 phase 4): the gate's interpreter-level
smoke tests could not run because `Interpreter::new()` spawns processes.

**Status 2026-08-03: slice 1 is done** (`src/runtime/io_sysinfo_host.rs`) — startup no longer forks
at all on Linux, and the Miri blocker is gone (`gc::soundness_smoke` runs under Miri). Slice 2, the
actual laziness, is still open; what remains is described below.

## What still happens today

`Interpreter::new()` → `init_io_environment` (`src/runtime/io_env.rs:86`) eagerly constructs
`$*DISTRO`, `$*PERL`, `$*RAKU`, `$*VM` and `$*KERNEL` and hoists them into the process-global env
base tier (`IMMUTABLE_BASE_DYNAMICS`, `src/runtime/mod.rs`). The remaining eager cost is:

| site | cost |
| --- | --- |
| `io_sysinfo_host::host_info()` | one cached `uname(2)` — cheap, but still paid when unread |
| `make_distro_instance` (macos arm), `io_sysinfo.rs` | `sw_vers` ×3 — **still three `fork`/`exec`s** |
| every `make_*_instance` | builds Instances, Versions, a 32-element signal array |

So macOS still pays three processes per `mutsu` start for data most programs never read, and every
platform pays the instance construction. The project's headline metric is startup time (0.04× raku),
which makes this the wrong place to be eager. The values are process constants, so they should be
**delayed until first access and then cached**, which is what Rakudo effectively does.

Note that laziness subsumes the macOS problem: `sw_vers` only runs if the program actually reads
`$*DISTRO`. Replacing it with a `SystemVersion.plist` read is the alternative, but it cannot be
verified from this workspace (no macOS host), and the current shell-out is at least known-correct.

## What slice 1 fixed (2026-08-03)

`uname -r` (×2, through two separate `OnceLock`s), `uname -m` and `hostname` are gone, replaced by
one cached `uname(2)` in `src/runtime/io_sysinfo_host.rs`; `hostname` was redundant all along
(`uname -n` is the same string). Verified with `strace`: a `mutsu -e 'say 1'` start now shows only
its own `execve` and the VM thread's `clone3`.

The module's fallback arm reads `/proc/sys/kernel/{osrelease,hostname}` instead of calling the
syscall, which is what Miri takes (it can read a file under `-Zmiri-disable-isolation`, but can
neither spawn a process nor call a foreign function). `local_timezone_offset_secs()` got the same
treatment — a `not(miri)` arm falling into its existing documented "offset unknown → 0" branch. With
those two, a real `Interpreter` builds and runs under Miri, so `gc::soundness_smoke`'s five tests
lost their `#[cfg_attr(miri, ignore)]` and the Miri job now covers the VM's real `gc_contents_mut`
call sites, not just the `Gc` primitive.

## Slice 2: delay construction (open)

Do not build these instances in `init_io_environment`; materialize on first read and cache. The
awkward part is *where* the hook goes: these live in the process-global base tier
(`crate::env::set_global_base`, a `OnceLock` set once at startup), and env lookup falls overlay →
parent chain → base tier with no miss hook. Either give the base tier a lazy-materialization entry
point, or leave the names out of it and let the dynamic-var read path (`get_dynamic_handle` and the
VM's dynamic read) construct-and-insert on demand. Whichever is chosen, the cache must stay
process-wide so repeated reads do not re-run the work, and it must be safe under the threaded lanes
that share the base tier.

## Pins

`roast/S02-magicals/KERNEL.t` and `DISTRO.t` require every field to be truthy, and
`roast/S32-io/IO-Socket-INET.t:337` matches `$*KERNEL.release` against `/Microsoft/` for WSL
detection — so the data must stay correct, not just cheap. `t/base-tier-magic-vars.t` pins the
base-tier hoisting itself, and `io_sysinfo_host`'s unit test pins the probed values against `/proc`.
