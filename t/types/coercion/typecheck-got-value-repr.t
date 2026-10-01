use Test;

plan 49;

# Rakudo words a failed store as `... expected R but got F (<.raku>)` for EVERY
# offending value: the value's type name, then its `.raku` (cut to 20 characters
# plus `...` when longer than 23). mutsu printed the tail only for a string, a
# number, a type object or an object, and named a List/Array/Hash/Pair/Range/Set/
# Seq/Complex/Version by its type alone. Each expected text below was measured on
# raku.

role R { }
class C { has R $.r }

sub got($v) {
    try C.new(:r($v));
    $!.message.subst('Type check failed in assignment to $!r; expected R but got ', '')
}

# The reported repro: collections.
is got((1, 2, 3)), 'List ((1, 2, 3))', 'a List';
is got([1, 2]), 'Array ([1, 2])', 'an Array';
is got({a => 1}), 'Hash ({:a(1)})', 'a Hash';
is got(()), 'List (())', 'the empty List';
is got([]), 'Array ([])', 'the empty Array';
is got({}), 'Hash ({})', 'the empty Hash';
is got((1, (2, 3))), 'List ((1, (2, 3)))', 'a nested List';
is got([[1, 2], [3]]), 'Array ([[1, 2], [3]])', 'a nested Array';
is got(<a b c>), 'List (("a", "b", "c"))', 'a List of strings quotes them';
is got(Empty), 'Slip (Empty)', 'a Slip';
is got((1, 2, 3).Seq), 'Seq ((1, 2, 3).Seq)', 'a Seq';
is got(Map.new((a => 1))), 'Map (Map.new((:a(1))))', 'a Map';

# Pair and Range.
is got((a => 1)), 'Pair (:a(1))', 'a Pair';
is got(1 => 2), 'Pair (1 => 2)', 'a Pair with an Int key';
is got((:a)), 'Pair (:a)', 'a True adverbial Pair';
is got((:!a)), 'Pair (:!a)', 'a False adverbial Pair';
is got(1..5), 'Range (1..5)', 'a Range';
is got(1^..^5), 'Range (1^..^5)', 'a Range with excluded ends';
is got(1..Inf), 'Range (1..Inf)', 'an infinite Range is not walked';
is got("a".."e"), 'Range ("a".."e")', 'a string Range';

# Other value kinds that have a pure `.raku`.
is got(1+2i), 'Complex (<1+2i>)', 'a Complex';
is got(v1.2.3), 'Version (v1.2.3)', 'a Version';
is got(/a/), 'Regex (/a/)', 'a Regex';
is got(Blob.new(1, 2)).substr(0, 4), 'Blob', 'a Blob still names its type';

# The scalar kinds spell themselves exactly as `.raku` does.
is got(True), 'Bool (Bool::True)', 'a Bool';
is got(1e0), 'Num (1e0)', 'a Num keeps its exponent';
is got(1.5e0), 'Num (1.5e0)', 'a fractional Num keeps its exponent';
is got(Inf), 'Num (Inf)', 'Inf';
is got(3.14), 'Rat (3.14)', 'a Rat with a terminating decimal reads as a decimal';
is got(1/3), 'Rat (<1/3>)', 'a Rat with a repeating decimal reads as a fraction';
is got(6/2), 'Rat (3.0)', 'a whole Rat keeps its `.0`';
is got(123456789.123456789), 'Rat (123456789.123456789)', 'a Rat is not rounded through a float';
is got(5.5.FatRat), 'FatRat (FatRat.new(11, 2))', 'a FatRat';
is got("a\nb"), 'Str ("a\nb")', 'a string is escaped';

enum Color <red green>;
is got(red), 'Color (Color::red)', 'an enum value';

# The value is named, not the `$` container a loop variable or `my $x = ...` holds it in.
{
    my $it = (1, 2, 3);
    is got($it), 'List ((1, 2, 3))', 'a List held in a scalar';
    for (1, 2, 3), [1, 2], {a => 1} -> $v {
        my $text = got($v);
        ok !$text.contains('$'), "a for variable holding a {$v.^name} is not itemized in the message";
    }
}

# Truncation applies to whatever text it is given.
is got([1 .. 30]), 'Array ([1, 2, 3, 4, 5, 6, 7...)', 'a long Array is cut to 20 characters';
is got({a => 1, b => 2}).substr(0, 6), 'Hash (', 'a Hash gets a tail';

# A collection holding objects renders each object by its own `.raku`.
class P { has $.x = 1 }
class Q { method raku { "Q-custom" } }
is got([P.new]), 'Array ([P.new(x => 1)])', 'an Array holding an object';
is got([Q.new, 2]), 'Array ([Q-custom, 2])', 'an Array holding an object whose class declares `raku`';
is got({a => P.new}), 'Hash ({:a(P.new(x => 1))})', 'a Hash holding an object';
is got((a => Q.new)), 'Pair (:a(Q-custom))', 'a Pair holding an object';

# Every route to the same failure agrees.
sub f(R $x) { }
try f([1, 2]);
is $!.message, q[Type check failed in binding to parameter '$x'; expected R but got Array ([1, 2])],
    'a parameter binding';
my R $v;
try $v = (1, 2);
is $!.message, 'Type check failed in assignment to $v; expected R but got List ((1, 2))',
    'a typed scalar assignment';
my @a of R;
try @a.push({x => 1});
is $!.message, 'Type check failed for an element of @a; expected R but got Hash ({:x(1)})',
    'an element of a typed array';

# The structured exception still carries the value itself.
try C.new(:r([1, 2]));
is-deeply $!.got, [1, 2], '.got is the offending value, not its repr';
