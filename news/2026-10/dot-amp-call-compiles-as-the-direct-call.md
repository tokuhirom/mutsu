# `$x.&f(args)` takes the direct-call path

`$x.&f(args)` is by definition `f($x, args)`, and the compiler already said so in a
comment, but compiled it as a call on the code variable `&f`: resolve `&f` at run
time, build a code object from the declaration, bind through the general binder.
That was ~50k instructions against ~4k for `f($x)`, and Red's `Red::Column` builds
two such calls (`$!attr.&inflator`, `.&deflator`) per construction. A plain
unqualified name now compiles as the `Expr::Call` it is equivalent to; that call
already hands a visible `my &f` or an enclosing lexical sub to the code-variable
form, so those keep their meaning (pinned in the new test, which behaves the same
under rakudo). Refs #15498.
