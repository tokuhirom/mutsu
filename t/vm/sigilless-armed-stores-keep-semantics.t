use v6;
use Test;

plan 6;

# A program that has bound a sigilless name arms the readonly/alias marker
# probes for every store; the probes must still catch the names that really
# carry a marker and leave all others alone.
my \z = 1;
my $plain = 10;
$plain = 11;
is $plain, 11, 'a plain scalar assigns after a sigilless binding exists';

dies-ok { EVAL 'my \y = 5; y = 6' }, 'assigning to a sigilless binding still dies';

my ($a, $b) = 1, 2;
$a = $b + 1;
is $a, 3, 'a destructured scalar assigns';

sub inv($n) {
    my ($i, $h) = 7, $n;
    my $t = $i div $h;
    $h = $i - $t * $h;
    $i = $h;
    $i
}
is inv(3), 1, 'declarations and stores in a sub body';

my $x := $plain;
$x = 12;
is $plain, 12, 'a bound scalar still writes through';

my $big = 2 ** 80;
$big = $big * 3;
is $big, 3 * 2 ** 80, 'a bigint store';
