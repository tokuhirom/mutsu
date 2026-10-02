use Test;

# A `next`/`last` that leaves a loop iteration runs the loop's NEXT/LEAVE
# phasers wherever it is written in the iteration's own frame (the
# LoopExitGuard of #10566), including the positions the retired static
# rewrite never reached. Each expectation was checked against rakudo.

plan 6;

sub trace(&loop) { my @*T; loop(); @*T.join(' ') }

is trace({ for 1..3 { NEXT @*T.push("n$_"); given $_ { when 2 { next } }; @*T.push("b$_") } }),
    'b1 n1 n2 b3 n3', 'next in a when body runs NEXT';

is trace({ for 1..3 { NEXT @*T.push("n$_"); try { next if $_ == 2 }; @*T.push("b$_") } }),
    'b1 n1 n2 b3 n3', 'next in a try body runs NEXT';

is trace({ for 1..3 { NEXT @*T.push("n$_"); do { next if $_ == 2 }; @*T.push("b$_") } }),
    'b1 n1 n2 b3 n3', 'next in a do block runs NEXT';

is trace({ for 1..3 { NEXT @*T.push("n$_"); $_ == 2 and next; @*T.push("b$_") } }),
    'b1 n1 n2 b3 n3', 'an expression-form next runs NEXT';

is trace({ for 1..3 { LEAVE @*T.push("l$_"); given $_ { when 2 { last } }; @*T.push("b$_") } }),
    'b1 l1 l2', 'last in a when body runs LEAVE';

# A `next` in a block passed to `map` ends the map's iteration, not the loop's.
is trace({ for 1..2 { NEXT @*T.push("n$_"); (1,).map({ next }); @*T.push("b$_") } }),
    'b1 n1 b2 n2', 'next in a map block is not the loop\'s';
