use Test;

# The subscript protocol of receivers with no shape is rows (ADR-11276 §9.154):
# `Nil` declares its own `AT-POS` and `AT-KEY`, a built-in type object answers
# `Any`'s, and a hash with no settled shape its type's associative rows.

plan 17;

is Nil.AT-POS(0).raku, 'Nil', 'Nil.AT-POS is Nil';
is Nil.AT-POS(3).raku, 'Nil', 'Nil.AT-POS of any index is Nil';
is Nil.AT-KEY('a').raku, 'Nil', 'Nil.AT-KEY is Nil';
nok Nil.EXISTS-POS(0), 'nothing exists in Nil (positional)';
nok Nil.EXISTS-KEY('a'), 'nothing exists in Nil (associative)';
is Nil<a>.raku, 'Nil', 'a Nil key subscript';

is Any.AT-KEY('a').raku, 'Any', 'Any.AT-KEY autovivifies';
nok Any.EXISTS-KEY('a'), 'Any.EXISTS-KEY';
is Int.AT-KEY('a').raku, 'Any', 'a built-in type object autovivifies';
nok Int.EXISTS-POS(0), 'a type object holds nothing';
dies-ok { Mu.AT-KEY('a') }, 'Mu is no Any: it has no AT-KEY';

class C { has $.x }
is C.new(x => 1)[0].x, 1, 'an instance is the one-element list of itself';
ok (C.new[0]:exists), 'its index 0 exists';
nok (C.new[1]:exists), 'no other index exists';

class MM is Map { }
my $mm = MM.new((b => 2));
is $mm.AT-KEY('b'), 2, 'a Map subclass reads its keys';
ok $mm.EXISTS-KEY('b'), 'a Map subclass has its keys';
nok ($mm<c>:exists), 'and only those';
