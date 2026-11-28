use v6;
use Test;

# A gather pull runs the body inline and then puts the puller's env back. It
# used to put back a flattened copy of the puller's whole scope chain, which
# cost a copy of every name in scope per pull (#19352). Whatever form the env
# is restored in, the puller must see exactly what it saw before, plus the
# writes the body made to its free variables.

plan 9;

my @Seen;
my $count = 0;
my \lz = gather for ^5 -> $i { $count++; @Seen.push($i); take $i * 10 };

is lz[0], 0, 'first pull';
is $count, 1, 'the body wrote an outer scalar';
is lz[2], 20, 'a later pull';
is @Seen, [0, 1, 2], 'the body pushed to an outer array';

sub pull-in-sub() {
    my $local = 'sub-local';
    my \inner = gather { take $local; take $local.uc };
    my @got = inner[0], inner[1];
    # The sub's own lexicals and its closures still resolve after the pulls.
    my &later = { $local ~ '!' };
    (@got, later()).flat.join('|')
}
is pull-in-sub(), 'sub-local|SUB-LOCAL|sub-local!', 'a sub sees its lexicals after a pull';

my $outer = 1;
sub nested($n) {
    # Gathers nested through recursion, each pulled from the other's body.
    return [$n] if $n <= 1;
    gather for nested($n - 1) -> $r { $outer++; take ($n, |$r) }
}
is nested(4).map(*.join(',')).join(' '), '4,3,2,1', 'a gather pulled from inside a gather';
is $outer, 4, 'nested bodies wrote the outer scalar once per level';

my %Memo;
my \squares = (^Inf).map: -> $x { %Memo{$x} //= $x * $x };
is squares[^4], (0, 1, 4, 9), 'a map pulled lazily writes an outer uppercase hash';
is %Memo.elems, 4, 'every pulled element was memoized';
