use Test;

# `last` / `next` inside a .reduce block are loop-control signals: `last` stops
# the reduction and returns the current accumulator; `next` skips the current
# step, keeping the accumulator. They must not escape as an X::ControlFlow error.

plan 5;

sub last-after-seven { last if $^a > 7; $^a + $^b }
is (2, 3, 4, 5).reduce(&last-after-seven), 9,
    'last in a reduce block returns the accumulator so far';

is (2, 3, 4, 5).reduce(-> $a, $b { last if $a > 7; $a + $b }), 9,
    'last in a pointy reduce block';

is (2, 3, 4, 5).reduce(-> $a, $b { next if $b == 4; $a + $b }), 10,
    'next in a reduce block skips the step, keeping the accumulator';

# plain reductions still work (no control flow)
is (1..5).reduce(&infix:<+>), 15, 'plain reduce still works';
is (2, 3, 4, 5).reduce({ $^a + $^b }), 14, 'plain block reduce still works';
