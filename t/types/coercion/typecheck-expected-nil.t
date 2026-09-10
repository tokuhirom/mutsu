use Test;

plan 6;

# The `.expected` attribute of a type-check exception is the expected type
# OBJECT, so a `expected => Nil` matcher (Nil's type object is special) works.

throws-like 'my Nil $a = 3', X::TypeCheck::Assignment, expected => Nil,
    'assignment to a Nil-typed scalar';

throws-like 'sub aa (Nil $a) { }; my $b = 3; aa($b)', X::TypeCheck::Binding,
    expected => Nil, 'binding to a Nil-typed parameter';

throws-like 'my Nil $a := 3', X::TypeCheck::Binding, expected => Nil,
    'binding (:=) to a Nil-typed scalar';

# Non-Nil expected types still compare correctly (Package-backed type objects).
throws-like 'my Int $a = "x"', X::TypeCheck::Assignment, expected => Int,
    'Int expected still matches';

# .expected identity / smartmatch against the real Nil type object.
{
    my $ex;
    { my Nil $a = 3; CATCH { default { $ex = $_ } } }
    ok $ex.expected === Nil, '.expected === Nil';
    ok $ex.expected ~~ Nil, '.expected ~~ Nil';
}
