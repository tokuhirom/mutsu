use Test;

plan 10;

# `for @a[i] { ... }` / `for %h<k> { ... }` topicalize a single lvalue element
# the same way `given @a[i] { ... }` does: `$_` aliases the element, so both
# reassignment (`.=`) and container mutation propagate back into it.

{
    my @a = "1", "2";
    for @a[1] { .=Int }
    is-deeply @a, ["1", 2], 'array element writeback via .=';
}

{
    my %h = a => "1";
    for %h<a> { .=Int }
    is-deeply %h, { a => 1 }, 'hash element writeback via .=';
}

{
    my @a = 1, 2, 3;
    for @a[1] { $_ = 99 }
    is-deeply @a, [1, 99, 3], 'array element writeback via direct assignment';
}

{
    my %h = a => { b => "1" };
    for %h<a><b> { .=Int }
    is-deeply %h<a><b>, 1, 'nested hash element writeback';
}

# Sibling elements are untouched.
{
    my @a = 1, 2, 3;
    for @a[1] { $_ = 99 }
    is @a[0], 1, 'sibling element before is untouched';
    is @a[2], 3, 'sibling element after is untouched';
}

# `last` still works (single-iteration loop).
{
    my @a = 1, 2, 3;
    for @a[1] { $_ = 99; last }
    is-deeply @a, [1, 99, 3], 'last inside the single iteration still writes back';
}

# A nested Array element is topicalized whole, not flattened, matching a
# plain scalar/variable topic (`for $x { ... }` over an Array-holding $x).
{
    my @a = [1, 2], [3, 4];
    my @seen;
    for @a[0] { @seen.push($_) }
    is-deeply @seen, [[1, 2],], 'a nested array element is one topic, not flattened';
}

# A slice index (`@a[range]` / `@a[list]`) yields several elements — it must
# NOT be routed through the single-element rewrite (regression: this collapsed
# the whole slice into one topicalized value, breaking roast
# S02-magicals/args.t's `for @*ARGS[1..^+@*ARGS] { .say }`).
{
    my @a = 1, 'two', 'three';
    my @seen;
    for @a[1 ..^ +@a] { @seen.push($_) }
    is-deeply @seen, ['two', 'three'], 'a range-slice index still iterates every element';
}

{
    my @a = 1, 'two', 'three';
    my @seen;
    for @a[1, 2] { @seen.push($_) }
    is-deeply @seen, ['two', 'three'], 'a list-slice index still iterates every element';
}
