use Test;

# ADR-11276 §9.92: `Cool.Real`, `Cool.Num`, `Cool.UInt`, `Mu.Int` and
# `Mu.Numeric` are rows, reached by every receiver whose own type declares
# none: a Range, a Seq, a Slip, a lazy or shaped array, a Blob, a Match, a
# StrDistance, a Stash and the type objects. The answers below are Rakudo's.

plan 34;

is "12".Real, 12, 'Str.Real parses';
isa-ok "1.5".Real, Rat, '... to the natural numeric type';
my $f = "x".Real;
isa-ok $f, Failure, 'a string that is no number is a Failure';
$f.so;
is [1, 2, 3].Real, 3, 'Array.Real is the element count';
is (a => 1, b => 2).Map.Real, 2, 'Map.Real is the pair count';
is (1..3).Real, 3, 'Range.Real';
is (1, 2).map(* + 1).Real, 2, 'Seq.Real';
is FatRat.new(1, 3).Real, FatRat.new(1, 3), 'FatRat.Real';
$f = ("abc" ~~ /b/).UInt;
isa-ok $f, Failure, 'Match.UInt reads the matched text';
$f.so;

is (1..3).Num, 3e0, 'Range.Num';
is slip(1, 2).Num, 2e0, 'Slip.Num';
ok Num.Num === Num, 'Num.Num is the type object';
throws-like { Int.Num }, X::Parameter::InvalidConcreteness, 'Int.Num needs an instance';
throws-like { Rat.Num }, X::Parameter::InvalidConcreteness, 'Rat.Num needs an instance';

is (5+0i).UInt, 5, 'a Complex with no imaginary part is a UInt';
$f = (1..Inf).UInt;
isa-ok $f, Failure, 'an infinite Range has no UInt';
$f.so;
is slip(1, 2).UInt, 2, 'Slip.UInt';

ok Int.Int === Int, 'Int.Int is the type object';
ok UInt.Int === UInt, 'UInt.Int is the type object';
throws-like { Str.Int }, X::Parameter::InvalidConcreteness, 'Str.Int needs an instance';
is (1..3).Int, 3, 'Range.Int';
throws-like { (1..Inf).Int }, Exception, 'an infinite Range has no Int';
is Blob.new(1, 2).Int, 2, 'Blob.Int is its length';
is Buf.new(3).Numeric, 1, 'Buf.Numeric is its length';
throws-like { DateTime.new(0).Int }, X::Multi::NoMatch, 'DateTime declares no Int';
my @lazy = 1, 2, 4 ... Inf;
$f = @lazy.Int;
isa-ok $f, Failure, 'a lazy array cannot count itself';
$f.so;
my @shaped[2] = 1, 2;
is @shaped.Numeric, 2, 'a shaped array numifies to its element count';
is FatRat.new(1, 3).Numeric, FatRat.new(1, 3), 'FatRat.Numeric';

{
    my $warned = 0;
    CONTROL { when CX::Warn { $warned++; .resume } }
    is Rat.Numeric, 0, 'a numeric type object numifies to its zero';
    is RatStr.Real, 0, 'a type object with no shape reaches Cool.Real';
    is Int.Real, 0, 'Int.Real';
    is Any.Real, 0, 'Any.Real is Mu\'s, not a Cool-only method';
    is $warned, 4, '... each with a warning';
}

my $s = "abc";
my $dist = $s ~~ tr/a/b/;
is $dist.Int, 1, 'a StrDistance is its edit distance';
