# An uncaught `warn` names the routine it ran in

The location line under an uncaught `warn` used to read `in block <unit>` of the
main script whatever routine the `warn` ran in. It now prints the innermost
backtrace frame, as Rakudo does (`in sub f at FILE line N`), and a method frame
reads `in method` (#13379).
