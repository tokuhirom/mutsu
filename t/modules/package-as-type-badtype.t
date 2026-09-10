use Test;

plan 8;

# Using a `package`/`module` (which is not type-like) to constrain a variable or
# parameter throws X::Syntax::Variable::BadType / X::Parameter::BadType, not a
# generic "not declared" error — a `package`/`module` IS declared, just not a type.

throws-like 'my package A {}; my A $a;', X::Syntax::Variable::BadType,
    'package used as variable type';
throws-like 'my module M {}; my M $a;', X::Syntax::Variable::BadType,
    'module used as variable type';
throws-like 'my package A {}; sub foo(A $a) { }', X::Parameter::BadType,
    'package used as parameter type';
throws-like 'my module M {}; sub foo(M $a) { }', X::Parameter::BadType,
    'module used as parameter type';

# A real type (class/role/grammar/built-in) is fine as a constraint.
{
    my class C { }
    my C $c;
    pass 'class is type-like enough to qualify a variable';
}
{
    sub takes(Int $x) { $x }
    is takes(5), 5, 'built-in type parameter works';
}
{
    my role R { }
    sub takes-r(R $x) { 'ok' }
    pass 'role is type-like enough to qualify a parameter';
}

# A genuinely undeclared type name is still an undeclared/invalid-type error,
# NOT BadType.
{
    my $err;
    try { EVAL 'sub f(NoSuchTypeXYZ $x) { }'; CATCH { default { $err = $_ } } }
    nok $err ~~ X::Parameter::BadType,
        'an undeclared type name is not reported as BadType';
}
