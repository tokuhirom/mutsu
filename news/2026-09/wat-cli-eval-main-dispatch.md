# WAT--CLI's use-ok test no longer dispatches an imported MAIN

`WAT--CLI`'s `t/00-use.rakutest` loaded the module successfully and passed its
`use-ok` assertion, but mutsu then treated the exported `MAIN` as the outer
test program's command-line entry point and printed `Usage:`. Rakudo keeps a
`MAIN` imported by the `EVAL` used by `use-ok` out of that outer dispatch.

The EVAL rollback still restores a loaded module's own routines so later
imports can use them, while removing only newly introduced `GLOBAL::MAIN`
candidates. Direct top-level imports of exported MAIN routines keep their
existing dispatch behavior.

Pinned by `t/lang/eval-imported-main.t`.
