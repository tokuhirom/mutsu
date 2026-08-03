# The Miri gate now watches the VM's real call sites, and startup stopped forking

ADR-0013 §4 phase 4 added a Miri job to defend the one thing the GC's `UnsafeCell` design depends
on: that `gc_contents_mut` takes an aliased `&mut` into a shared container node without violating
provenance. But the job could only run the `Gc` primitive tests. The five interpreter-level tests in
`src/gc/soundness_smoke.rs` — the ones that make a *real* `Interpreter` execute a Raku program so
Miri watches the actual mutation sites — were `#[cfg_attr(miri, ignore)]`, because
`Interpreter::new()` died before reaching any container code. The ADR itself warned that a
primitive-only run "mostly re-proves std's `UnsafeCell` guarantee".

The blocker had nothing to do with the GC. `$*KERNEL` and `$*DISTRO` were built eagerly at every
startup, and building them **shelled out**: `uname -r` (twice, through two separate `OnceLock`s in
two different functions), `uname -m`, and `hostname`. Miri cannot spawn a process
(`unsupported operation: can't call foreign function 'posix_spawnattr_init'`), so startup aborted.

## One syscall instead of three processes

`src/runtime/io_sysinfo_host.rs` replaces all of it with a single cached `uname(2)`. The struct
already carries every field that was being fetched separately — `release`, `machine`, and
`nodename`, which is exactly the string the `hostname` command prints — so the duplication
disappears along with the fork/exec pairs. Verified with `strace`: a `mutsu -e 'say 1'` start now
issues only its own `execve` and the VM thread's `clone3`, where it previously spawned three
subprocesses. Every `prove` test process and every `mzef` subprocess pays that start-up cost, so
this is a small win repeated constantly.

The values are unchanged, which is the part that matters: `roast/S02-magicals/KERNEL.t` and
`DISTRO.t` require every field to be truthy, and `roast/S32-io/IO-Socket-INET.t` matches
`$*KERNEL.release` against `/Microsoft/` to detect WSL. A unit test pins the probed values against
`/proc/sys/kernel/{osrelease,hostname}`.

## The arm Miri takes

Miri can neither spawn a process nor call a foreign function, but with `-Zmiri-disable-isolation` it
*can* read a file. So the module's fallback arm — the one used when `libc` is unavailable, and the
one `cfg(miri)` selects — reads `/proc/sys/kernel/osrelease` and `/proc/sys/kernel/hostname` and
takes the architecture from `std::env::consts::ARCH`, all verified equal to the `uname` output on
Linux. `local_timezone_offset_secs()` got the same treatment: a `not(miri)` arm that falls into the
function's already-documented "offset could not be determined → 0" branch rather than calling
`libc::localtime_r`.

With those two, a real `Interpreter` builds and runs under Miri. The five smoke tests lost their
`cfg_attr` and pass: an aliased array push through `:=`, an aliased hash insert, a captured array
seeing a later push, a `.^set_name` on a mixin reaching every alias (the write that was the last
`Arc`-backed container mutation until #5771), and a self-referential array that makes the collector
run its own `&mut` fixup paths.

## Two CI steps, because only one half can be leak-checked

The Miri job now runs the subset in two steps. The primitives keep Miri's leak check on — a `Gc`
node outliving its test is a collector bug worth failing for. The interpreter-level step adds
`-Zmiri-ignore-leaks`, which is not a loosening of the gate: a whole interpreter intentionally
leaves memory live at exit (process-lifetime statics such as the env base tier and interned symbols,
plus uncollected reference cycles — precisely what the cycle collector exists to reclaim on demand
rather than at teardown), and those reports would drown the provenance errors the job exists to
catch. Aliasing and provenance checking are unaffected by the flag. The VM-level step measures ~245s
of interpretation for its five tests, against a 45-minute job budget.

What remains is the deeper half of `todo/tickets/magic-vars-should-be-built-lazily.md`: these
instances should be built on *first access*, not at startup at all. macOS still runs `sw_vers` three
times per start, and laziness — not another syscall substitution — is the fix for that, since a
program that never reads `$*DISTRO` should pay nothing. The awkward part is that the values live in
the process-global env base tier, which has no miss hook; the ticket records the two candidate
designs.
