use Test;

plan 4;

# Declaring a typed variable without a sigil on the name (`my Int a`) is
# X::Syntax::Malformed, not a bare X::AdHoc.

throws-like 'my Int a;', X::Syntax::Malformed,
    'sigilless name after a type (with ;)',
    message => { m/'Malformed my (did you mean to declare a sigilless'/ };
throws-like 'my Int a', X::Syntax::Malformed,
    'sigilless name after a type (no ;)';

# Well-formed declarations still work.
{
    my Int $x = 5;
    is $x, 5, 'a normal typed scalar declaration works';
}
{
    my \y = 9;
    is y, 9, 'a sigilless \\name declaration works';
}
