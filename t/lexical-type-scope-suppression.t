use Test;

# A `my`-scoped class is lexically scoped to its enclosing block. Once the
# block exits, the name is no longer a visible *type* (not just no longer a
# bare term). Previously only the bare-word path honoured the suppression, so
# an out-of-scope `my class` still resolved as a type constraint.

plan 7;

# After the block, `A` is undeclared as a type, not silently accepted.
# (Previously the out-of-scope `my class` still resolved as a type and the
# `my A $x` declaration was accepted with no error.)
throws-like '{ my class A is Any { } }; my A $x;', X::Comp,
    'out-of-scope my class is not a type for a variable declaration';

# A freshly-declared package used as a type is still BadType (the suppression
# fix must not mask the in-scope "insufficiently type-like" diagnostic).
throws-like 'my package P { }; sub g(P $x) { }', X::Parameter::BadType,
    'in-scope package used as parameter type is BadType';

# A `my package A` declaration shadows a same-named class whose lexical scope
# has already exited: the package becomes the active `A`, so `my A $x` reports
# X::Syntax::Variable::BadType (package is insufficiently type-like) rather than
# resolving to the dead, out-of-scope class.
throws-like '{ my class A is Any { } }; my package A { }; my A $x;',
    X::Syntax::Variable::BadType,
    'my package shadows an out-of-scope same-named class (variable type)';

# A class that is still in scope resolves normally.
{
    my class InScope { method tag { 'ok' } }
    is InScope.new.tag, 'ok', 'in-scope my class resolves and works';
}

# `my package`/`my module` did not participate in the same lexical-scope
# bookkeeping as `my class`/`my role`: its bare env binding stayed visible
# after the enclosing block/EVAL exited, so a later, UNRELATED `my package A`
# (in a different EVAL) permanently un-suppressed the name via
# `shadow_suppressed_type_with_package` above and never got re-suppressed.
# That corrupted every later BadType check against the same short name for
# the rest of the program (roast/S32-exceptions/misc.t, found while working
# todo/tickets/vendor-real-test-module.md).
{
    my $first_err;
    try { EVAL 'my package A {}; my A $a;'; CATCH { default { $first_err = $_ } } }
    my $second_err;
    try { EVAL 'my package A {}; sub foo(A $a) { }'; CATCH { default { $second_err = $_ } } }
    ok $first_err ~~ X::Syntax::Variable::BadType,
        'first, unrelated EVAL of "my package A" still raises BadType';
    ok $second_err ~~ X::Parameter::BadType,
        'a second, independent EVAL of "my package A" still raises BadType '
        ~ 'too (not silently accepted because the first EVAL un-suppressed A)';
}

# `my package`/`my module` inside a bare block must not stay resolvable
# outside it either (mirrors `my class`'s own scoping, exercised above). A
# fresh name (not `A`), so this only pins the bare-block scoping itself and
# not the separate, deeper cross-EVAL env-retention gap the earlier
# assertions' repeated use of `A` would otherwise also trip.
throws-like '{ my package PkgBlockScope { } }; PkgBlockScope', X::Undeclared::Symbols,
    'my package declared in a bare block does not outlive the block';
