use v6;
use Test;

# An EVAL reaches the lexical scope it is written in. The process-wide
# "something may read lexicals by name" latch used to be set by any EVAL, so
# every parameter and store was mirrored into the env; now only the chunk that
# contains (or nests) the EVAL mirrors its locals (#14337). These pin that the
# EVAL's own view stays complete.

plan 8;

my $x = 5;
sub peek-outer { EVAL '$x' }
$x = 9;
is peek-outer(), 9, 'EVAL in a sub sees the current value of a file-level lexical';

sub with-param($n) { EVAL '$n + 1' }
is with-param(4), 5, 'EVAL sees its own sub\'s parameter';

sub with-local($p) { my $q = $p * 2; $q ~= 'x'; EVAL '$q' }
is with-local(3), '6x', 'EVAL sees a local assigned after declaration';

my $y = 'a';
$y ~= 'b';
is EVAL('$y ~ "c"'), 'abc', 'top-level EVAL sees a concatenation-assigned lexical';

my @seen;
for 1..2 { my $z = $_ * 2; @seen.push(EVAL('$z')) }
is-deeply @seen, [2, 4], 'EVAL in a loop body sees the per-iteration lexical';

sub unrelated($n) { $n + 1 }
sub ev-elsewhere($c) { EVAL $c }
my $s = 0;
$s = unrelated($s) for ^50;
is $s, 50, 'calls of a routine in a program that has an EVAL elsewhere still work';
is ev-elsewhere('$s * 2'), 100, 'EVAL in a routine reads a file-level lexical updated by calls';

sub assigns-outer { EVAL '$s = 7' }
assigns-outer();
is $s, 7, 'EVAL in a routine can assign a file-level lexical';
