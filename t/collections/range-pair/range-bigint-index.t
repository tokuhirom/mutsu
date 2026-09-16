use v6;
use Test;

# `($!start..$!end)[$n]`'s fast i64 arithmetic path used to accept any
# GenericRange whose endpoints were merely `is_numeric` and convert them via
# `to_f64() as i64` -- which silently saturates for a whole number beyond
# i64::MAX/MIN (an IPv6-scale BigInt, as Net::Netmask's `$!start`/`$!end`
# are) and silently truncates a fractional Num/Rat endpoint, in both cases
# answering the wrong element instead of falling back to the BigInt-safe
# `value_to_list` expansion. See
# https://github.com/tokuhirom/mutsu/issues/8588.

plan 12;

my $start = 2**70; # far beyond i64::MAX
my $end = $start + 10;
my $r = $start..$end;

is $r[0], $start, 'index 0 answers the exact BigInt start';
is $r[2], $start + 2, 'a small positive index adds exactly, no i64 saturation';
is $r[10], $end, 'the last valid index answers the exact BigInt end';
is $r[11], Nil, 'past the end answers Nil, not a wrapped/saturated value';

is-deeply $r[2..4], ($start + 2, $start + 3, $start + 4),
    'a Range slice index computes each element exactly';
is-deeply $r[(2, 5)], ($start + 2, $start + 5),
    'a list slice index computes each element exactly';

my $excl = $start^..$end;
is $excl[0], $start + 1, 'an excluded-start GenericRange still indexes exactly';

# The same fast path also mis-handled a whole-numbered-looking but
# fractional Num/Rat range, truncating `1.5` to `1` instead of bailing to
# the .succ-based element expansion.
my $frac = 1.5 .. 5.5;
is $frac[0], 1.5, 'a fractional Num range keeps its fractional first element';
is $frac[1], 2.5, 'and steps by .succ (+1), not by truncated-int arithmetic';

my $rat = 3/2 .. 11/2;
is $rat[0], 3/2, 'a fractional Rat range keeps its fractional first element';

# A whole-numbered Num/Rat endpoint still takes the fast path correctly.
my $whole_num = 1e0 .. 5e0;
is $whole_num[2], 3, 'a whole-numbered Num range indexes as an integer range';

my $whole_rat = (4/2) .. (10/2);
is $whole_rat[1], 3, 'a whole-numbered Rat range indexes as an integer range';
