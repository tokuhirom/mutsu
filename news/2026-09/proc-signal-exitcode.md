# A signal-killed `Proc` now reports `exitcode = 0`, like rakudo

A process killed by a signal has no exit status at all: `waitpid` reports it as
*signalled*, not exited, and Rust's `ExitStatus::code()` is `None` there. mutsu
passed that `None` straight through as `-1`, so every signal-killed child came
back as `exitcode = -1`:

```raku
my $p = Proc::Async.new: $*EXECUTABLE, "-e", "sleep 60";
my $started = $p.start;
await $p.ready;
$p.kill: SIGTERM;
my $res = await $started;
say $res.exitcode, " ", $res.signal;
```

mutsu said `-1 15`; rakudo says `0 15`. Rakudo derives both numbers from the raw
wait status — `exitcode` is its high byte, `signal` its low one — so a signal
death reports `exitcode = 0` and carries the information in `.signal` alone.
Anything that inspected a killed process's `.exitcode` got the wrong number, and
`run`/`shell` diverged the same way.

The five places that turned a finished child's `ExitStatus` into a `Proc`
(`run`, `shell`, the `run(:in, ...)` live-proc finalization, `IO::Pipe.close`
and `Proc::Async`'s wait thread) each open-coded `status.code().unwrap_or(-1)`
plus a `#[cfg(unix)]` `signal()` block. They now share one
`builtins_system::exit_status_parts()` helper, which reports `0` for a signal
death and keeps `-1` for the case it was actually meant for — a child whose exit
status could not be read at all.

Normalising the exit code to `0` removes the only evidence a signal-killed `Proc`
had that it was unsuccessful, so the two consumers that read `exitcode` alone
were taught to consult `.signal` as well: sinking such a `Proc` still throws
`X::Proc::Unsuccessful` (rakudo throws there too, reporting
`exit code: 0, signal: 15`), and `Proc.Bool` is still `False` (rakudo:
`$!exitcode == 0 && $!signal == 0`). A child that genuinely exits non-zero is
untouched and still reports its own code — this is not "always report 0".

Pinned by `t/io/proc-signal-exitcode.t`, which covers `run`, `shell` and
`Proc::Async` for a signal death, a non-zero exit and a clean exit, plus the
sink-context and `Bool` behaviour. It passes unchanged under rakudo.
