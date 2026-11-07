use Test;

# From Math::Sequences (A000009): every pull of a lazy gather compared a
# rendering of each captured outer value to detect writes. Rebinding and
# in-place mutation of captured variables must still be visible afterwards,
# and a big captured collection must not make the pulls crawl.

plan 7;

my %big = (^3000).map({ "k$_" => $_ });
my @big = ^3000;
my $count = 0;
my $last = 0;

my @g = lazy gather for 1..* -> $n {
    $count++;
    $last = $n;
    take $n * 2 + %big<k1> + @big[2] - 3;
}

is @g[^3].List, (2, 4, 6), 'values from a gather that captures big collections';
is $count, 3, 'the body ran once per pulled element';
is $last, 3, 'a rebound captured scalar is visible outside';

# An in-place mutation of a captured hash is visible outside.
my %seen;
my @h = lazy gather for 1..* -> $n { %seen{$n} = $n * $n; take $n }
is @h[^4].List, (1, 2, 3, 4), 'elements';
is %seen.keys.sort(+*).List, (1, 2, 3, 4), 'a captured hash written by the gather body';

# A captured array that the body pushes to.
my @log;
my @a = lazy gather for 1..* -> $n { @log.push: $n; take $n }
@a[^3];
is @log.List, (1, 2, 3), 'a captured array pushed to by the gather body';

# Many pulls over a big captured collection stay cheap enough to finish.
my $total = 0;
my @s = lazy gather for 1..* -> $n { take $n + @big.elems - 3000 }
$total += @s[$_] for ^300;
is $total, ([+] 1..300), 'repeated pulls over a big captured array';
