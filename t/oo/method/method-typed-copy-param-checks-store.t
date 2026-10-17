use Test;

# A method's typed `is copy` parameter is its own variable of that type: a
# store into it is type-checked, as for a sub (#13783). Checked against rakudo.

plan 4;

class C {
    method m(Int $x is copy) { $x = 's'; $x }
    method ok(Int $x is copy) { $x = 5; $x }
    method n(Int :$x is copy) { $x = 's'; $x }
    method nil(Int $x is copy) { $x = Nil; $x }
}

throws-like { C.m(1) }, X::TypeCheck::Assignment,
    'a store of the wrong type into a positional is-copy parameter dies';
is C.ok(1), 5, 'a matching store is kept';
throws-like { C.n(:x(1)) }, X::TypeCheck::Assignment,
    'a named is-copy parameter is checked too';
is C.nil(1).raku, 'Int', 'a Nil store resets it to its type object';
