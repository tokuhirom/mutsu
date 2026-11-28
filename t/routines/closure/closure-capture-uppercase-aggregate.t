use v6;
use Test;

# An uppercase initial behind an `@`/`%` sigil names an ordinary variable, not
# a type: a closure captures such a variable exactly when it names it, shares
# its container with the declaring scope, and never captures the ones it does
# not name (#19324).

plan 8;

my @Cache;
my $push = -> $x { @Cache.push($x) };
$push($_) for 1..3;
is @Cache, [1, 2, 3], 'a pointy block pushes to an outer uppercase array';

my %Memo;
sub fib($n) { %Memo{$n} //= $n < 2 ?? $n !! fib($n - 1) + fib($n - 2) }
is fib(40), 102334155, 'a memo hash through //= in a recursive sub';
is %Memo.elems, 41, 'every value was memoized';

our @Pkg-Seq = 1, 2, 4 ... *;
sub nth($n) { (^$n).map({ @Pkg-Seq[$_] }).list }
is nth(5), (1, 2, 4, 8, 16), 'a map closure in a sub reads an our sequence';

my @Taken = gather for ^4 { take @Cache[$_ % 3] * 10 };
is @Taken, [10, 20, 30, 10], 'a gather body reads an outer uppercase array';

my @Shared = 0 xx 4;
await (^4).map: -> $i { start { @Shared[$i] = $i * $i } };
is @Shared, [0, 1, 4, 9], 'start blocks write an outer uppercase array';

my @Late = <x>;
my &later = { @Late.join };
@Late = <y z>;
is later(), 'yz', 'a closure sees a later reassignment of an uppercase array';

my @Unused = 1..1000;
my &plain = { 7 * 6 };
is plain(), 42, 'a closure that does not name an uppercase array still runs';
