use Test;

# #9009: `.sort(*.method)` / `.sort({ .method })` -- the "simple mapper"
# Schwartzian-transform fast path for a bare 0-arg method call on the topic
# -- silently stopped sorting when an element was itself a `ContainerRef`
# (e.g. an inner Array itemized by being pushed into an outer Array, exactly
# what `@outer.push(@inner)` does). The fast path dispatched the key method
# directly on the un-dereferenced `ContainerRef`, which a generic fallback
# answered as "1" for every element regardless of its real size -- so every
# Schwartzian key compared equal and the sort silently left the input order
# untouched. Found via the `Graph` ecosystem distribution's
# `weakly-connected-components`, whose components are built exactly this
# way (`@components.push(@component)`) and then `.sort(*.elems)`ed.
#
# See also t/collections/transform/sort-inline-comparator-container-elements.t,
# which pins the analogous `{ $^a <=> $^b }` two-arg comparator path against
# the same underlying container-promotion mechanism (there via `.grep`, here
# via itemization on push).

plan 4;

my @components;
for [<a b c d e f>], [<g>], [<h i j>] -> @src {
    my @component;
    for @src -> $x { @component.push($x); }
    @components.push(@component);
}

is-deeply @components.sort(*.elems)>>.elems.List, (1, 3, 6),
    '.sort(*.elems) orders itemized inner arrays by their real size';
is-deeply @components.sort({ .elems })>>.elems.List, (1, 3, 6),
    '.sort({ .elems }) (explicit block form) does the same';
is-deeply @components.sort(*.elems).reverse>>.elems.List, (6, 3, 1),
    '...and a subsequent .reverse still sees the correctly-sorted order';

# The already-working non-mapper forms (a `my`-bound intermediate, or a
# 2-arity comparator) must be unaffected -- this is a regression pin, not a
# behavior change for those.
is-deeply @components.sort({ my $e = .elems; $e })>>.elems.List, (1, 3, 6),
    'the equivalent explicit-return block form still sorts correctly';
