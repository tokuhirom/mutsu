use v6;
use Test;

# Math::Root: a closure sequence with a CLOSURE endpoint runs to its endpoint
# however big its elements get; only an endpoint-less / value-endpoint
# sequence may stop its eager prefix and continue lazily.

plan 3;

my $n = 2 * 10**6002;
my @s = (10**3001, { ($_ + $n div $_) div 2 } … { $_ * $_ <= $n and ($_ + 1) ** 2 > $n });
ok @s.elems > 2, 'the sequence is not cut short at the first big element';
ok @s.tail * @s.tail <= $n < (@s.tail + 1) ** 2, 'it ends at the isqrt of $n';
is (min (10**3001, { ($_ + $n div $_) div 2 } … { $_ * $_ <= $n and ($_ + 1) ** 2 > $n })).chars,
   3002, 'min over it sees the converged element';
