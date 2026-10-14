# EVAL writes through to the caller's `$_`

`EVAL q[$_ = "b"]`, `EVAL q[s[\d] += 5]` and `EVAL q[$_++]` now change the caller's
topic like rakudo. The EVAL unit's tail value used to travel through `$_` (`SetTopic`) and
the interpreter restored the saved topic afterwards, which undid the snippet's own writes.
The tail now stays on the VM stack (`Compiler::unit_tail_on_stack`) and the restore is gone.
