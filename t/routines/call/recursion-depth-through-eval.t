use Test;
use MONKEY-SEE-NO-EVAL;

plan 2;

# A call that goes through `EVAL` nests many VM dispatch activations per Raku
# level. A debug build once spent ~140 KB of native stack per activation (every
# arm of the one opcode `match` kept its own locals, #12299), so recursion
# through `EVAL` died with "Too deep recursion" at ~900 levels; the dispatch is
# now split into groups and 1200 levels fit with room to spare.
my $max = 0;
sub erec($n) {
    $max = $n;
    return $n if $n >= 1200;
    EVAL 'erec(' ~ ($n + 1) ~ ')';
}
is erec(1), 1200, 'recursion through EVAL reaches 1200 levels';
is $max, 1200, 'every level ran';
