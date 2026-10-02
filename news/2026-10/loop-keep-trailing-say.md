# Loop body ending in say/print runs KEEP, not UNDO

A loop body whose last statement was a statement-form `say`/`print`/`put`/`note` captured `Nil` as the iteration value, so `UNDO` ran instead of `KEEP`. These statements return `True`, and `expand_loop_phasers` now captures that, matching Rakudo (#10579).
