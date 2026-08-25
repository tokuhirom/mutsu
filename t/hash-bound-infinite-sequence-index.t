use Test;

# An infinite sequence bound into a hash value must stay indexable through the
# hash. Indexing `%h{$key}[$i]` used to divide by zero, because the bound Seq
# reached the subscript path without its lazy generator, so the element read
# saw an empty container. Reading two neighbouring far-out elements and
# dividing them is the shape that first surfaced it (a Fibonacci-ratio loop),
# so it is the shape pinned here.

plan 6;

my @fib   = 1, 1, * + * ... Inf;
my @lucas = 1, 3, * + * ... Inf;

# Direct indexing of the lazy arrays themselves still works.
is @fib[10], 89, 'infinite sequence indexes directly';
is @lucas[10], 199, 'second infinite sequence indexes directly';

my %sequences;
%sequences<f> := @fib;
%sequences<l> := @lucas;

# The same element, reached through the hash binding.
is %sequences<f>[10], 89, 'infinite sequence indexes through a hash binding';
is %sequences<l>[10], 199, 'second sequence indexes through a hash binding';

# Dividing two far-out neighbouring elements converges on 1/phi. The divide
# itself is the regression: the denominator used to read back as 0.
is-approx %sequences<f>[100] / %sequences<f>[101], 0.6180339887498949,
    'ratio of neighbouring far-out elements through the hash binding';

# Reached through an iterated key rather than a literal one, which is how the
# original report hit it.
my @ratios;
for %sequences.keys.sort -> $s {
    @ratios.push(%sequences{$s}[100] / %sequences{$s}[101]);
}
is-approx @ratios.sum / @ratios.elems, 0.6180339887498949,
    'both sequences converge when indexed via an iterated hash key';
