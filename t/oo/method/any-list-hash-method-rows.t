use Test;
plan 23;

# Any.list and Any.hash are rows (ADR-11276 §9.65); expectations checked against raku.
is 5.list.raku, '(5,)', 'Int.list';
is "ab".list.raku, '("ab",)', 'Str.list';
is 1.5.list.raku, '(1.5,)', 'Rat.list';
is True.list.raku, '(Bool::True,)', 'Bool.list';
is 5.list.WHAT.raku, 'List', 'the list is a List';
is Date.new("2020-01-02").list.elems, 1, 'Date.list';
is (a => 1).list.raku, '(:a(1),)', 'Pair.list';
is <a b>.list.raku, '("a", "b")', 'List.list';
is {a => 1}.list.raku, '(:a(1),)', 'Hash.list is its pairs';
is (set <a>).list.elems, 1, 'Set.list';
is (1..3).list.raku, '(1, 2, 3)', 'Range.list';

is (a => 1, b => 2).hash.sort.raku, '(:a(1), :b(2)).Seq', 'List.hash';
is [:a(1)].hash.raku, '{:a(1)}', 'Array.hash';
my $it = $(:a, :b);
is $it.hash.sort.raku, '(:a(Bool::True), :b(Bool::True)).Seq', 'an itemized list is read as its elements';
is (a => 1).hash.raku, '{:a(1)}', 'Pair.hash';
is (1 => 2).Seq.hash.raku, '{"1" => 2}', 'Seq.hash';
is {a => 1}.hash.raku, '{:a(1)}', 'Hash.hash';
is Nil.hash.raku, '{}', 'Nil.hash';
is Int.hash.raku, '{}', 'a type object hashes to the empty hash';
throws-like { 5.hash }, X::Hash::Store::OddNumber, 'Int.hash is an odd initializer';
throws-like { "ab".hash }, X::Hash::Store::OddNumber, 'Str.hash is an odd initializer';
throws-like { Date.new("2020-01-02").hash }, X::Hash::Store::OddNumber, 'Date.hash is an odd initializer';
is (1..4).hash.sort.raku, '("1" => 2, "3" => 4).Seq', 'Range.hash pairs the elements';
