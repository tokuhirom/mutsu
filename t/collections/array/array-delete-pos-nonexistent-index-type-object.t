use Test;

# `Array.DELETE-POS` of an index past the end answers the element type object
# (`Any`, `Int` for `my Int @a`), not `Nil` (#16087). Checked against rakudo.

plan 7;

my @a = 42, 666;
my Int @ai = 42, 666;

is-deeply @a.DELETE-POS(4), Any, 'untyped Array: past the end answers Any';
is-deeply @ai.DELETE-POS(4), Int, 'Array[Int]: past the end answers Int';
ok !@a.EXISTS-POS(4), 'the missing index still does not exist';
is @a.elems, 2, 'a missing index leaves the array alone';
is-deeply @a[9]:delete, Any, 'the :delete adverb agrees (untyped)';
is-deeply @ai[9]:delete, Int, 'the :delete adverb agrees (typed)';
is-deeply @ai.DELETE-POS(0), 42, 'a typed element answers its value';
