use Test;

# #9161: `.unique` / `.repeated` bucket Seq, Slip, Enum, Set/Bag/Mix,
# Version, routines, type objects, Nil, Ranges, allomorphs, ObjAt and
# metaobjects by their `===` identity class, instead of scanning every value
# already seen. The answers are rakudo's.

plan 18;

enum Color <Red Green>;
my $s = (1, 2).Seq;
my $t = (1, 2).Seq;
is ($s, $t, $s).unique.elems, 2, 'a Seq is identical only to itself';
is-deeply (Red, Green, Red, Color::Red).unique.List, (Red, Green), 'enum values';
is (set(1, 2), set(2, 1), set(3), SetHash.new(1, 2)).unique.elems, 3,
    'a Set by its elements and its mutability';
is (bag(1, 1, 2), bag(1, 2, 1), bag(2)).unique.elems, 2, 'a Bag by its counts';
is ((a => 1).Mix, (a => 1).Mix, (a => 2).Mix).unique.elems, 2, 'a Mix by its weights';
is (v1.2, v1.2, v1.02, v1.3).unique.map(~*).join(' '), '1.2 1.02 1.3',
    'a Version by its spelling';
sub f {}
sub g {}
is (&f, &g, &f).unique.elems, 2, 'a routine by its name';
is-deeply (Int, Str, Int, Nil, Nil).unique.List, (Int, Str, Nil), 'type objects and Nil';
is (1..3, 1..3, 1..4).unique.elems, 2, 'an integer Range by its bounds';
is (Empty, Empty, |()).unique.elems, 0, 'Empty flattens away';
is (1.WHICH, 1.WHICH, 2.WHICH).unique.elems, 2, 'an ObjAt by its identity text';
is (Int.HOW, 1.HOW, Str.HOW).unique.elems, 2, 'a metaobject by the type it describes';
my @a = 1, 2;
is (@a, @a, [1, 2]).unique.elems, 2, 'an Array is identical only to itself';
is-deeply <1 1 2 a a>.unique.List, <1 2 a>, 'allomorphs';
is (IntStr.new(1, "1"), 1, IntStr.new(1, "01")).unique.elems, 3,
    'an allomorph by its Str part too';
my $x = 3;
is-deeply ($x, 3, $x).unique.List, (3,), 'a container is its value';
is-deeply (Red, Green, Red).repeated.List, (Red,), 'repeated: enum values';
is (set(1), set(1), set(2), v1, v1).repeated.elems, 2, 'repeated: Sets and Versions';
