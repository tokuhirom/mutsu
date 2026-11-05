# RakuAST::ArgList from-invocant-list, a real invocant, replace-args, set-arg-at-pos

`RakuAST::ArgList.from-invocant-list($colon-apply)` builds an argument list
from a colon-call's operands: the first operand is the invocant, the rest are
the arguments. `invocant` now reads it back (it answered the
`RakuAST::Expression` type object for every list); like rakudo's private
attribute, it is left out of `.raku`, `args`, `visit-children` and the
lowering. `replace-args(@args)` and `set-arg-at-pos($pos, $arg)` change the
arguments of the shared node in place and answer `Nil`; a negative position
counts from the end and one past the end pads with `Mu` holes. All measured
against rakudo 2026.09.
