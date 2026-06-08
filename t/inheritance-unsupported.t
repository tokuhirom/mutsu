use Test;

plan 4;

# Inheriting from a `package` (or module) — which exists but does not support
# inheritance — is X::Inheritance::Unsupported, not an unknown-parent error.
throws-like 'my package A { }; my class B is A { }', X::Inheritance::Unsupported,
    'a class cannot inherit from a my package';
throws-like 'package A { }; class B is A { }', X::Inheritance::Unsupported,
    'a class cannot inherit from a package';

# A genuinely undeclared parent is still an unknown-parent error.
throws-like 'class B is NoSuchParent { }', X::Inheritance::UnknownParent,
    'inheriting from an undeclared name is an unknown-parent error';

# Ordinary class inheritance still works.
{
    my $r = EVAL 'class A { method m { 41 } }; class B is A { }; B.new.m';
    is $r, 41, 'ordinary class inheritance still works';
}
