use Test;

plan 4;

# Assigning Nil through a sigilless name resets the aliased Scalar container
# to its default (its `of` type object, else Any), as `$x = Nil` does.
my $z = 1;
for ($z,) -> \q { q = Nil }
is $z.raku, 'Any', 'Nil through a sigilless loop param resets an untyped Scalar';

my Int $y = 1;
for ($y,) -> \q { q = Nil }
is $y.raku, 'Int', 'Nil through a sigilless loop param resets a typed Scalar to its type object';

my $x = 1;
my \v = $x;
v = Nil;
is $x.raku, 'Any', 'Nil through a sigilless alias of a variable resets it';

my Int $b = 1;
my \c := $b;
c = Nil;
is $b.raku, 'Int', 'Nil through a bound sigilless name resets a typed Scalar';
