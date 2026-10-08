use Test;

# raku and Str on the collections are rows of the method table (ADR-11276 §9.38);
# the answers are the ones the cascades gave before.

plan 33;

# --- raku
is [1, 2, 3].raku, '[1, 2, 3]', 'Array.raku';
is (1, 2, 3).raku, '(1, 2, 3)', 'List.raku';
is [1, [2, 'a']].raku, '[1, [2, "a"]]', 'a nested Array';
is (:a(1), :b(2)).Seq.raku, '(:a(1), :b(2)).Seq', 'Seq.raku';
is { a => 1 }.raku, '{:a(1)}', 'Hash.raku';
is Map.new((a => 1)).raku, 'Map.new((:a(1)))', 'Map.raku';
is (a => 1).raku, ':a(1)', 'Pair.raku';
is (1 => (2 => 3)).raku, '1 => 2 => 3', 'a Pair with a Pair value';
is \(1, 2, :a(3)).raku, '\(1, 2, :a(3))', 'Capture.raku';

class Pt { has $.x; method raku { "Pt<$!x>" } }
is [Pt.new(x => 1)].raku, '[Pt<1>]', 'an element with its own raku';

# --- Str
is [1, 2, 3].Str, '1 2 3', 'Array.Str';
is (1, 2, 3).Str, '1 2 3', 'List.Str';
is (1, 2, 3).Seq.Str, '1 2 3', 'Seq.Str';
is { a => 1 }.Str, "a\t1", 'Hash.Str';
is (a => 1).Str, "a\t1", 'Pair.Str';
is (1..3).Str, '1 2 3', 'Range.Str';
is \(1, 2, :a(3)).Str, "1 2 a\t3", 'Capture.Str';
is (1..*).list[^3].Str, '1 2 3', 'a sliced lazy list';
class Nm { method Str { 'nm' } }
is [Nm.new, Nm.new].Str, 'nm nm', 'an element with its own Str';
is [Nm.new].Stringy, 'nm', 'Stringy shares the rendering';
is (1, 2).Seq.Stringy, '1 2', 'Seq.Stringy';
throws-like { (1/0, 2).Str }, X::Numeric::DivideByZero, 'a zero-denominator rational dies in .Str';
is [<a b>, <c d>].Str, 'a b c d', 'nested arrays flatten in .Str';
is [].Str, '', 'an empty Array';
is ().Str, '', 'an empty List';

# --- Capture.gist
is \(1, 2).gist, '\(1, 2)', 'Capture.gist';
is \(:a(1)).gist, '\(:a(1))', 'a named Capture.gist';
is \().gist, '\()', 'an empty Capture.gist';

# --- the shapes that keep the cascade
is (1, 2).Seq.map(* + 1).raku, '(2, 3).Seq', 'a Seq from map';
is (my @a = 1, 2).raku, '[1, 2]', 'a bound array';
is Array.raku, 'Array', 'a type object';
is "{ (1,2).Seq }", '1 2', 'interpolation';

# vim: expandtab shiftwidth=4
