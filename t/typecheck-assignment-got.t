use Test;

plan 5;

# X::TypeCheck::Assignment exposes `.expected` as the expected type object and
# `.got` as the offending value (not its type name).

throws-like 'my Int $x = "foo"', X::TypeCheck::Assignment,
    got => 'foo', expected => Int, symbol => '$x';

throws-like 'my Int $x = "foo"', X::TypeCheck::Assignment,
    message => /:s expected Int, got Str/;

# A different expected type / value.
throws-like 'my Str $s = 42', X::TypeCheck::Assignment,
    got => 42, expected => Str, symbol => '$s';

# `.expected` is the type object (matched here via the type-object matcher).
throws-like 'my Str $s = 42', X::TypeCheck::Assignment, expected => Str;

# A valid typed assignment still works.
lives-ok { my Int $ok = 5; $ok }, 'valid typed assignment lives';
