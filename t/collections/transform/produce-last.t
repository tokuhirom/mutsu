use v6;
use Test;

plan 8;

# Rakudo's produce emits each accumulator value only after the NEXT reducer
# call completes, so `last` drops the pending value too, and `next` skips
# that step's emit while keeping the accumulator (PLAN §8.12).

is-deeply ((2,3,4,5).produce: -> $a, $b { last if $a > 7; $a + $b }).List,
    (2, 5), 'last on the third call keeps only the first two values';
is-deeply ((2,3,4).produce: -> $a, $b { last; $a + $b }).List,
    (), 'last on the first call yields the empty Seq';
is-deeply ((2,).produce: -> $a, $b { last; $a + $b }).List,
    (2,), 'a single-element list never calls the reducer';
# (Eagerly reified via assignment: consuming the lazy Seq inside a routine
# call would let the block's `next` escape into the consumer's loop in Rakudo.)
my @next-skipped = (2,3,4,5,6).produce: -> $a, $b { next if $b == 4; $a + $b };
is-deeply @next-skipped.List,
    (2, 5, 10, 16), 'next skips the step and keeps the accumulator';
is-deeply ((2,3,4,5).produce: -> $a, $b { $a + $b }).List,
    (2, 5, 9, 14), 'plain produce is unchanged';
isa-ok ((2,3,4,5).produce: -> $a, $b { last if $a > 7; $a + $b }), Seq,
    'produce with last still returns a Seq';
is-deeply (produce -> $a, $b { last if $a > 7; $a + $b }, (2,3,4,5)).List,
    (2, 5), 'the function form follows the same rule';
is-deeply ([\+] (2,3,4,5)).List, (2, 5, 9, 14), 'triangle reduce is unchanged';

done-testing;
