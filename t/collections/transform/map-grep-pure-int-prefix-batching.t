use v6;
use Test;

plan 9;

# A deferred `.map` whose callback is a pure Int expression is run ahead of a
# `for` that pulls it one element per iteration. None of that is observable.

my @got;
for (1..40).map({ $_ + 1 }) { @got.push($_) }
is-deeply @got, [2..41], 'for over a pure Int map yields every element in order';

@got = ();
for (1..40).map(*+1) { @got.push($_); last if $_ == 5 }
is-deeply @got, [2, 3, 4, 5], 'last stops the loop; the rest of the source is not needed';

is-deeply (1..100000).map(*+1).head(3).list, (2, 3, 4), 'head of a long WhateverCode map';
is-deeply (1..100000).map({ $_ * 2 }).head(1).list, (2,), 'head(1) of a long block map';
is-deeply (1..50).map(*+1).map(*+1).head(5).list, (3, 4, 5, 6, 7), 'chained pure maps';

# An element that is not an Int is mapped when it is reached, not before: the
# earlier elements are processed first, and the failure comes at its turn.
my @seen;
my $died = False;
try {
    for (1, 2, 3, 4, 5, 6, 7, 8, "x", 10).map({ $_ + 1 }) { @seen.push($_) }
    CATCH { default { $died = True } }
}
is-deeply @seen, [2, 3, 4, 5, 6, 7, 8, 9], 'elements before a non-Int are consumed first';
ok $died, 'the non-Int element dies when it is reached';

# A consumer that writes the source between pulls still sees its own writes
# mapped (a live Array is not run ahead).
my @src = 1..10;
my @out;
for @src.map({ $_ + 1 }) {
    @out.push($_);
    @src[7] = 100 if $_ == 2;
}
is-deeply @out, [2, 3, 4, 5, 6, 7, 8, 101, 10, 11], 'a write to a later source element is seen';

# Callbacks that are not pure keep their per-element interleaving.
my @log;
for (1..3).map({ @log.push("m$_"); $_ }) { @log.push("f$_") }
is-deeply @log, ['m1', 'f1', 'm2', 'f2', 'm3', 'f3'], 'an impure callback interleaves with the loop body';
