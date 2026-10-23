use v6;
use Test;

# A loop's per-iteration state sync and its entry reset visit the `state`
# declarations of the loop's own body (#11347). The answers must not change
# when the chunk holds many unrelated ones, nor when the loop nests others.

plan 8;

state $outer-a = 10;
state $outer-b = 20;
state $outer-c = 30;

my @seen;
for ^3 { state $k = 0; $k++; @seen.push($k) }
is-deeply @seen, [1, 2, 3], 'a state in the loop body accumulates across iterations';

my @again;
for ^2 { state $k = 100; $k++; @again.push($k) }
is-deeply @again, [101, 102], 'a second loop statement starts its own state afresh';

my @nested;
for ^2 -> $i {
    state $outer = 0;
    $outer += 10;
    for ^2 { state $inner = 0; $inner++; @nested.push($outer + $inner) }
}
is-deeply @nested, [11, 12, 21, 22], 'nested loops keep separate states';

is $outer-a + $outer-b + $outer-c, 60, 'chunk-level states are untouched by the loops';

sub counter {
    my $r = 0;
    for ^3 { state $n = 0; $n++; $r = $n }
    $r
}
is counter(), 3, 'a routine loop state accumulates';
is counter(), 3, '... and a fresh loop statement per call restarts it';

my $c;
for ^5 { $c = -> { 1 } }
is $c(), 1, 'a closure made in the loop works';

my $total = 0;
for ^4 { state $acc = 0; $acc += $_; $total = $acc }
is $total, 6, 'a state accumulates a value read from the loop topic';
