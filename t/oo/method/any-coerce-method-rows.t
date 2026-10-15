use Test;

# ADR-11276 §9.110: `Any`'s `List`, `list`, `Array`, `Slip`, `Supply` and
# `Seq`, `Mu.Capture`, `Pair.Pair`, the numbers' `Complex` and `Int.isNaN` are
# rows, and the receivers the deleted `methods_0arg/coercion.rs` arms
# answered reach them.

plan 31;

# A type object lists as itself.
is Int.List.raku, (Int,).raku, 'Int.List';
is Int.Slip.raku, 'slip(Int,)', 'Int.Slip';
is Int.Array.raku, [Int,].raku, 'Int.Array';
is Any.list.raku, (Any,).raku, 'Any.list';
isa-ok Int.Supply, Supply, 'Int.Supply';
is Int.Seq.elems, 1, 'Int.Seq';

# A closed shape with no row of its own.
is (1..3).List, (1, 2, 3), 'Range.List';
is (1..3).Slip.raku, 'slip(1, 2, 3)', 'Range.Slip';
isa-ok (1..3).Supply, Supply, 'Range.Supply';
is Buf.new(1, 2).List, (1, 2), 'Buf.List';
is (a => 1).Array.raku, [a => 1].raku, 'Pair.Array';
is "x".IO.List.elems, 1, 'IO::Path.List';
ok (1..Inf).List.is-lazy, 'an infinite Range lists lazily';
is ("a".."c").List, <a b c>, 'a string Range lists its strings';

# A `Capture` is its positionals.
is \(1, 2, :a(3)).Array.raku, [1, 2].raku, 'Capture.Array';
is \(1, 2, :a(3)).Slip.raku, 'slip(1, 2)', 'Capture.Slip';
is \(1, 2, :a(3)).Supply.list, (1, 2), 'Capture.Supply';

# A `Supply` lists its values; a lazy list stays lazy.
is Supply.from-list(1, 2).list, (1, 2), 'Supply.list';
ok (1, 2, 3).lazy.Seq.is-lazy, 'a .lazy list stays lazy through .Seq';
is (gather { take 1 }).List, (1,), 'a gather lists its values';

# `Capture`.
is Any.Capture.raku, \().raku, 'Any.Capture';
is 1.5.Capture.raku, \(:numerator(3), :denominator(2)).raku, 'Rat.Capture';
throws-like { Int.Capture }, X::Cannot::Capture, 'Int.Capture';

# `Pair.Pair` of the type object.
is Pair.Pair.raku, 'Pair', 'Pair.Pair';

# `isNaN`: `Int`'s, and no `Str`.
is Int.isNaN, False, 'Int.isNaN';
is Bool.isNaN, False, 'Bool.isNaN';
throws-like { "a".isNaN }, X::Method::NotFound, 'Str.isNaN';

# `Complex` of a type object.
throws-like { Int.Complex }, Exception, 'Int.Complex';
throws-like { Num.Complex }, Exception, 'Num.Complex';

# No built-in `Range` method on a `Str` or a `List`.
throws-like { "abc".Range }, X::Method::NotFound, 'Str.Range';
throws-like { [1, 2].Range }, X::Method::NotFound, 'Array.Range';
