use Test;

plan 6;

# `range_elems_f64` (the element-count helper backing `Range ~~ Numeric` and
# the WhateverCode range-index placeholder, e.g. `$range[*-1]`) used to
# convert each GenericRange endpoint to f64 independently and subtract:
# `end.to_f64() - start.to_f64() + 1.0`. Once both endpoints exceed f64's
# 52-bit mantissa and are close together (e.g. two BigInts ~10 apart around
# 2**70), they round to the *same* float, collapsing a genuinely non-zero
# element count to 1. See https://github.com/tokuhirom/mutsu/issues/8591.

my $start = 2 ** 70;
my $end = $start + 10;
my $r = $start .. $end;

is $r ~~ 11, True, 'a BigInt-scale inclusive Range smart-matches its exact element count';
is $r ~~ 1, False, 'and not the f64-collapsed (wrong) count of 1';

my $r-excl-end = $start ..^ $end;
is $r-excl-end ~~ 10, True, 'an excluded-end BigInt-scale Range counts one fewer element';

my $r-excl-start = $start ^.. $end;
is $r-excl-start ~~ 10, True, 'an excluded-start BigInt-scale Range counts one fewer element';

my $r-excl-both = $start ^..^ $end;
is $r-excl-both ~~ 9, True, 'an excluded-both-ends BigInt-scale Range counts two fewer elements';

# A small, ordinary Range must keep working exactly as before.
is (1..5) ~~ 5, True, 'a plain small Range still smart-matches its element count';
