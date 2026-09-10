use Test;

plan 7;

# A multi-param `@`-sigil for-loop parameter de-itemizes the chunk element,
# just like Raku binds it: `for $@n, Any -> @a, $T` makes `@a` the underlying
# array (elems=4), not a one-element array wrapping the itemized value (elems=1).

my @n = <a b c d>;

for $@n, Any -> @a, $T {
    is @a.elems, 4,                 'multi-param @a de-itemizes (elems)';
    is @a[1], "b",                  'multi-param @a single index';
    is-deeply @a[1, 2], ("b", "c"), 'multi-param @a slice index';
    is $T, Any,                     'second param bound';
}

# Two array params, each de-itemized.
my @x = <a b>;
my @y = <c d>;
for $@x, $@y -> @a, @b {
    is-deeply (@a[0], @b[1]), ("a", "d"), 'two @-params both de-itemize';
}

# Single-param binding still works (regression guard).
for $@n -> @a {
    is @a.elems, 4,                 'single-param @a de-itemizes';
}

# Plain flat iteration with two scalar params is unaffected.
my @pairs = 1, 2, 3, 4;
my @seen;
for @pairs -> $p, $q { @seen.push: "$p-$q" }
is-deeply @seen, ["1-2", "3-4"],    'two scalar params chunk correctly';
