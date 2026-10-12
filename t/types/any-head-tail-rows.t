use Test;

# `head` and `tail` with and without a count are method-table rows
# (ADR-11276 §9.84): `List.head`, `Map.head`, `Seq.head`, `List.tail` and
# `Array.tail` on their owners, `Any.head`/`Any.tail` (`self.list.head`) for
# every other receiver, a type object included. `Mu` is no `Any`.

plan 25;

class P { has $.head = 'attr' }
class Q { }

my @a = 1, 2, 3;
is @a.head, 1, 'Array.head';
is @a.tail, 3, 'Array.tail';
@a.head = 10;
is @a[0], 10, 'Array.head is the element container';
my @holes; @holes[2] = 5;
ok @holes.head === Nil, 'a hole at the front reads Nil';
is (4, 5, 6).tail, 6, 'List.tail';
is (1..5).head, 1, 'Range.head';
is (1^..5).head, 2, 'an excluded-start Range.head';
ok (5..1).head === Nil, 'an empty Range.head is Nil';
is ('a'..'c').head, 'a', 'a Str Range.head';
is (1..*).head, 1, 'an infinite Range.head';
is (1, 2, 3).map(* * 2).head, 2, 'Seq.head';
is (1, 2, 3).map(* * 2).tail, 6, 'Seq.tail';
is %(a => 1).head.key, 'a', 'Map.head';
is \(7, 8).tail, 8, 'a Capture tails its positionals';
is Blob.new(3, 4).head, 3, 'a Blob heads its first byte';
ok "foo".IO.head ~~ IO::Path, 'an IO::Path heads itself';
ok Date.new(2020, 1, 1).head ~~ Date, 'a Date heads itself';
is P.new.head, 'attr', "a user class's own accessor wins";
ok Q.new.tail ~~ Q, 'a user-class instance tails itself';
ok Int.head === Int, 'a type object heads itself';
ok Int.tail === Int, 'a type object tails itself';
is Int.head(2).elems, 1, 'a type object heads one element';
ok List.tail === List, 'the List type object tails itself';
is (1, 2, 3).tail(2), (2, 3), 'List.tail(2)';
throws-like { Mu.head }, X::Method::NotFound, 'Mu.head has no candidate';
