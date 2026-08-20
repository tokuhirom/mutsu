# A `%*ENV` write after a thread spawn is invisible to a later default-env child: verified fixed, audited, and regression-pinned

`todo/deep/env-var-write-invisible-to-spawn-after-a-thread.md` reported that once
any OS thread had ever been spawned via mutsu's `clone_for_thread`/
`worker_pool::submit*` machinery (used by every live tap: signals, sockets,
`Supply.interval`, `Proc::Async`), a subsequent `%*ENV<key> = val` write stopped
being visible to a *later* spawned child that relies on default OS-level env
inheritance (no explicit `:ENV`/`:env`) — persistently, not as a timing race.

## Status: the fix already landed, but had no dedicated regression test

Investigating this ticket found that PR `7ea201824` ("fix(run/shell/procasync):
stop relying on env inheritance after any thread spawn", 2026-08-15) had already
fixed the three spawn sites the ticket names: `Proc::Async.start()`
(`src/runtime/native_proc_async.rs`), `run()`, and `shell()`
(`src/runtime/builtins_system_run.rs`). All three now explicitly rebuild the
child's environment from mutsu's own `%*ENV` hash (`cmd.env_clear()` + a loop of
`cmd.env(k, v)`) whenever no `:ENV`/`:env` override is given, instead of relying
on `Command::spawn()`'s default OS-level inheritance. A `ProcOptions.env_explicit`
flag distinguishes "no `:env` given" from "explicitly `:env({})`" so an
intentional empty override still clears the child's environment.

Both of the ticket's repros (spawn via `Proc::Async` with a `.stdout.tap()`, and
spawn via `Supply.interval(...).tap()`) were re-run against current `main` and
both now correctly print `meows`, matching `raku`.

## The isolation experiment (direction 2, cursory)

A minimal Rust program outside mutsu entirely — `std::thread::spawn(|| {})`,
joined or left running, then `std::env::set_var(...)` then
`Command::new("sh")...` — did **not** reproduce the hazard at all: the child
always saw the freshly set env var, whether the spawned thread had already been
joined or was still alive. This means the corruption is **not a generic Rust/
libc/OS hazard that any multi-threaded Rust program hits** from
`std::env::set_var` after a `thread::spawn` — it is specific to something in
mutsu's own threading setup (`worker_pool`, `clone_for_thread`, GC mutator
registration, or signal-handling setup on those threads). The root cause was not
pinned down further (out of scope per the ticket's own guidance, which favored
direction 1 regardless of root cause), but this result confirms direction 1 —
never relying on OS-level default env inheritance for `%*ENV`, building the
child's env explicitly from mutsu's own hash instead — is the correct general
fix rather than a narrow workaround for an unavoidable upstream hazard.

## Audit of remaining `Command::new()`/`.spawn()` sites

Every other spawn site in `src/` was checked:

- `src/runtime/io_sysinfo.rs` (`sw_vers`, `cmd /C ver` for `$*DISTRO`/`$*KERNEL`)
  and `src/runtime/system_introspect.rs` (`hostname`, `kill`/`taskkill`) are
  OS-introspection helpers. They take no `:ENV`/`:env` parameter, are not part
  of Raku's env-passing contract, and their output does not depend on `%*ENV`
  content — a `%*ENV` write made after a thread spawn cannot affect what
  `sw_vers` or `hostname` report. They rely on default inheritance only for
  locating the binary via `PATH` and reading platform info, which is set once
  at process start (before any user code could have raced past a thread
  spawn) and is unaffected by this hazard in any way a test could observe.
  These sites were left as-is; fixing them to explicitly pass `%*ENV` would add
  no observable correctness benefit and risks accidentally dropping `PATH` if
  mutsu's own `%*ENV` hash ever diverged from the OS environment.
- No other `Command::spawn()` call sites exist.

## Regression coverage

- `roast/S29-os/system.t` (already whitelisted) exercises exactly this
  scenario for `Proc::Async`-spawned threads: its "run and shell's :env"
  subtest runs after an earlier `Proc::Async` invocation earlier in the file,
  so `make roast` already re-verifies this on every push.
- Added `t/env-write-after-thread-spawn.t` as a dedicated local pin covering
  *both* thread-spawn paths from the ticket (`Proc::Async` and
  `Supply.interval`) against *both* `run()` and `shell()`, independent of
  the roast file's specific ordering.

No code changes were needed; the ticket is closed as verified-fixed with
added regression coverage.
