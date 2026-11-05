use v6;
use Test;

plan 9;

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

# Declarations in a loop body keep working once a sigilless name exists: a
# name that carries a marker is still refused after a redeclaration, a name
# that does not is a plain slot write.
my @seen;
for 1..3 -> $i {
    my $t = $i * 2;
    my $u = $t + 1;
    @seen.push: $u;
}
is-deeply @seen, [3, 5, 7], 'declarations in a loop body';

sub shadow() {
    my \w = 10;
    my $r = w + 1;
    $r
}
is shadow(), 11, 'a sigilless binding inside a sub and a declaration after it';

sub redeclare() {
    my $z = 5;      # a plain `z` next to the file-level `\z`
    $z = $z + 1;
    $z
}
is redeclare(), 6, 'a scalar named like a sigilless binding elsewhere';
