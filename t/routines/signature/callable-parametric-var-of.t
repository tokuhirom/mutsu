use Test;

plan 5;

# An unassigned variable typed `Callable[T]` (or `my T &f`) holds the
# `Callable[T]` type object, which keeps its parameter for `.of` (#16449).
my Callable[Int] $x;
is $x.of.raku, 'Int', 'my Callable[Int] $x; .of';
my Int &f;
is &f.of.raku, 'Int', 'my Int &f; .of';
my Int:D &g;
is &g.of.raku, 'Int:D', 'the smiley is kept';
is $x.raku, 'Callable[Int]', '.raku names the parametrized type object';
my Callable[Int] $y = sub (--> Int) { 4 };
is $y(), 4, 'an assigned Callable[Int] still runs';
