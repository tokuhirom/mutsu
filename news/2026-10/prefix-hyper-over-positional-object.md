# Prefix hyper over a `Positional` non-`Iterable` object

`-« $carray` answered `(0,)` in mutsu; rakudo gives the elementwise `(-1e0, -2e0)`.
The prefix hyper form goes through `__mutsu_hyper_prefix`, which never saw the
object-resolution the infix form gained with the `X::Multi::Ambiguous` fix: a
unary hyper has no second operand to tie its candidates on, so such an object
now hypers as its `.list`. The infix ambiguity message also names the operand's
real type (`NativeCall::Types::CArray`) instead of `Any`.
