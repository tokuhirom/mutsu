use Test;

# A typed array's constraint is its element type: a lone value initializing
# it (`my Int @a = "x"`) is its one element, and the refusal names the
# element, as rakudo's does. A `:D` element handed its type object carries
# rakudo's hint about Nil.

plan 4;

throws-like { my Int @a = "x" }, X::TypeCheck::Assignment,
    message => 'Type check failed for an element of @a; expected Int but got Str ("x")',
    'a lone initializer value';
throws-like { my Num @n = "x" }, X::TypeCheck::Assignment,
    message => 'Type check failed for an element of @n; expected Num but got Str ("x")',
    '...of another type';
throws-like { my Int:D @d = Int }, X::TypeCheck::Assignment,
    message => 'Type check failed for an element of @d; expected Int:D but got Int (Int) (perhaps Nil was assigned to a :D which had no default?)',
    'a :D element given its type object';
throws-like { my Int:D @e; @e[0] = Int }, X::TypeCheck::Assignment,
    message => /'(perhaps Nil was assigned to a :D which had no default?)'/,
    '...also through an element store';
