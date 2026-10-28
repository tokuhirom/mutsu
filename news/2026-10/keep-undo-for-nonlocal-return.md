# UNDO runs when a non-local return passes through a KEEP/UNDO routine

A `return` aimed at an outer routine that unwinds through a routine
declaring `KEEP`/`UNDO` used to be treated as a successful exit because its
value was defined, so `KEEP` ran. It is an unsuccessful exit for the routine
being left, so `UNDO` runs now, matching Rakudo.
