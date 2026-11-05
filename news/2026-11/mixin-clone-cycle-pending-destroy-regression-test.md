# A pending-DESTROY instance holding a `but`-mixed clone of itself exits cleanly (regression test)

The debug-build `Gc::drop strong-count underflow` panic at thread teardown (an instance whose
`but`-mixed clone refers back to it, with a `DESTROY` pending) no longer reproduces on `main`:
the issue's script, a loop of such guards and a variant under `start` all exit 0 with an empty
stderr on a debug build, with `MUTSU_GC` on and off. `t/oo/mixin-clone-cycle-pending-destroy-exits-clean.t`
runs the script in a child process and pins the exit status and stderr. Closes #15243.
