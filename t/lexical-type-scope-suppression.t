use Test;

# A `my`-scoped class is lexically scoped to its enclosing block. Once the
# block exits, the name is no longer a visible *type* (not just no longer a
# bare term). Previously only the bare-word path honoured the suppression, so
# an out-of-scope `my class` still resolved as a type constraint.

plan 4;

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
