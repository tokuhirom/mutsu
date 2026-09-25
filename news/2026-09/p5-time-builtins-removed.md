# Perl 5 `times` / `localtime` / `gmtime` are no longer mutsu builtins

`times`, `localtime` and `gmtime` are Perl 5 routines; rakudo reports
"Undeclared routine" for all three. mutsu nevertheless provided them as
builtins, and `times` was worse than a stray builtin: the parser recognised it
as a hard-coded 0-arg term *before* consulting user declarations. A module that
defines its own `times` -- P5times does, as `proto sub times(|)` with a
`times(Scalar:U)` candidate -- was therefore shadowed: `times(Scalar)` parsed as
`times()` followed by a postfix call on the builtin's List result and died with
"No such method 'CALL-ME' for invocant of type 'List'".

All three builtins are gone: the `times` term in
`parser/primary/ident/term_literals.rs`, their entries in the listop/expr-listop
predicate lists, the EVAL known-routine name, and `builtins/functions/time.rs`
itself. A user or module definition now dispatches normally, and a bare call
with no declaration is `X::Undeclared::Symbols`, as in rakudo. The local test
that exercised the builtins (`t/types/temporal/temporal-time.t`) was removed;
`t/routines/p5-time-routines-not-core.t` pins the new behaviour (issue #9418).

P5times `t/01-basic.t` gets past the shadowing but still stops at test 3 on
`nqp::getrusage`, which is tracked separately in #9348.
