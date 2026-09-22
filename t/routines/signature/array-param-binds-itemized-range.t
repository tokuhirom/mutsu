use v6.d;
use Test;

plan 3;

# A non-slurpy `@`-sigil parameter binds directly to a Positional argument,
# ignoring any `$`-itemization the caller's variable carries: a Range read
# out of a scalar must bind to `@x` exactly as the same Range literal would.
# Found via Math::Polynomial::Chebyshev's `chebyshev-rec(Str, UInt, @x,
# %cheb)` multi candidate, invoked with a Range forwarded through a `$`-sigil
# parameter -- the itemized Range bound to `@x` as a single wrapped element
# instead of its five elements, doubling the nesting of a later hyper op.

sub sum-doubled(@x) {
    return (2 <<*<< @x).sum;
}

is sum-doubled((^5)), 20, 'array param sums a literal Range correctly';

my $r = (^5);
is sum-doubled($r), 20,
    'array param sums a Range read from a scalar variable the same way';

sub identity(@x) { @x }

sub forward($x) { identity($x) }

is-deeply forward((^5)).Array, [0, 1, 2, 3, 4],
    'array param binds a Range forwarded through another sub\'s scalar param without extra nesting';
