use Test;

# #12958: a type object samples itself through `Any.pick`/`Any.roll`
# (`self.list.pick`), with or without a count; an enum type object samples
# its values; `Mu` is no `Any`, so it has neither these nor `Any`'s other
# list methods.

plan 14;

enum E <ea eb ec>;

is-deeply Int.roll(2).List, (Int, Int), 'Int.roll(2)';
is-deeply Any.roll(3).List, (Any, Any, Any), 'Any.roll(3)';
is-deeply Int.pick(2).List, (Int,), 'Int.pick(2) picks its one element';
my $t = Str;
is-deeply $t.roll(2).List, (Str, Str), 'a type object in a variable rolls';
is E.pick(*).elems, 3, 'an enum type object picks from its values';
ok E.roll ~~ E, 'an enum type object rolls one of its values';
is Bool.roll(4).elems, 4, 'Bool.roll(4)';
is "abc".pick, "abc", 'a Str is its own one-element list';
is-deeply "abc".roll(2).List, ("abc", "abc"), 'a Str rolls itself';

throws-like { Mu.roll(2) }, X::Method::NotFound, 'Mu.roll(2)';
throws-like { Mu.tail }, X::Method::NotFound, 'Mu.tail';
throws-like { Mu.tail(2) }, X::Method::NotFound, 'Mu.tail(2)';
throws-like { Mu.unique }, X::Method::NotFound, 'Mu.unique';
throws-like { Mu.minmax }, X::Method::NotFound, 'Mu.minmax';
