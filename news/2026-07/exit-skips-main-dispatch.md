# `exit` no longer runs MAIN afterwards

```raku
sub MAIN(Str :$r!) { }
say "mainline";
exit 0;
```

raku prints `mainline` and exits 0. mutsu printed `mainline`, then `Usage:`, and
exited **2** — it dispatched MAIN after the mainline had already called `exit`.
`exit` terminates the process in Raku, so nothing may run after it.

The mainline's `exit` sets `Interpreter::halted`, but `run()` called
`dispatch_main` unconditionally (apart from the existing `explicit_run_main`
guard). It now also skips when `halted` is set. The MAIN behaviour of a program
that does *not* exit is unchanged: a satisfiable MAIN still runs, and an
unsatisfiable one still prints usage and exits 2.

## Why it mattered beyond the exit code

This was found while fixing the real-dist compatibility sweep's own probe. The
sweep asks "does this module load?" by running `mutsu -I <dist>/lib -e 'use M'`,
and a dist that exports a `MAIN` had it dispatched by that very probe:

- `RakudoContainerfileBuilder::CLI` printed its usage and exited non-zero →
  bucketed `runtime_error`.
- `Raku::Pod::Render`'s `InstallAtomHighlighter` exports a `MAIN` that shells out
  to `npm`/`git`, so under the sweep's no-net sandbox it hung → bucketed
  `timeout`.

Neither is a mutsu bug; both modules load. The natural fix is to make the probe
`use M; exit 0`, which is what raku users would expect to suppress MAIN — except
that did not work in mutsu until this change. `scripts/dist-compat-sweep.py` now
uses that probe, so those two dists classify honestly.

Pinned by `t/exit-skips-main-dispatch.t` (6 `is_run` subtests covering: exit
before an unsatisfiable MAIN, exit before a satisfiable MAIN, a non-zero exit
code, MAIN still dispatching without an exit, usage + exit 2 still reported
without an exit, and an imported MAIN — fixture `t/lib/ExitMainFixture.rakumod`).
All 6 identical under raku.
