use Test;

plan 8;

# A type on a `&` variable parametrizes the Callable it holds (`&f.of`), it is
# not a constraint on a value of that type (#16439).
my Int &foo = sub (--> Int) {};
is &foo.of.raku, 'Int', 'my Int &foo holds a Callable returning Int';

my Int:D &bar = sub (--> Int:D) {};
is &bar.of.raku, 'Int:D', 'the smiley is kept in .of';

my Int:D &baz = sub (--> Int:D) { 42 };
is baz(), 42, 'the stored routine is callable';

my Int &g = sub (--> Int) { 3 };
is g(), 3, 'assigned routine runs';

throws-like { my Int &h = sub (--> Str) { "a" } }, X::TypeCheck::Assignment,
    'a routine with another return type is rejected';
throws-like { my Int &h = 5 }, X::TypeCheck::Assignment,
    'a plain Int is not a Callable';

my Int &u;
is &u.raku, 'Callable[Int]', 'an unassigned typed & variable is the Callable[Int] type object';

my &plain = sub { 1 };
is plain(), 1, 'an untyped & variable is unchanged';
