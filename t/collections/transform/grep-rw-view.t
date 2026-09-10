use Test;

# `.grep` returns an rw view of the source array: mutating the grepped
# elements (via a for loop or hyper postfix) writes back to the original.

plan 10;

# for loop over grep result, implicit $_
{
    my @a = 1..6;
    for @a.grep(* %% 2) { $_++ }
    is-deeply @a, [1, 3, 3, 5, 5, 7], 'for @a.grep(...) { $_++ } writes back';
}

# statement-modifier for over grep result
{
    my @a = 1..6;
    $_++ for @a.grep(* %% 2);
    is-deeply @a, [1, 3, 3, 5, 5, 7], 'postfix for over grep writes back';
}

# pointy block with `is rw`
{
    my @a = 1..6;
    for @a.grep(* %% 2) -> $x is rw { $x++ }
    is-deeply @a, [1, 3, 3, 5, 5, 7], 'for grep -> $x is rw writes back';
}

# block-form predicate
{
    my @a = 1..6;
    for @a.grep({ $_ > 3 }) { $_ *= 10 }
    is-deeply @a, [1, 2, 3, 40, 50, 60], 'block predicate grep writes back';
}

# hyper postfix increment still works
{
    my @a = 1..6;
    @a.grep(* %% 2)>>++;
    is-deeply @a, [1, 3, 3, 5, 5, 7], '@a.grep(...)>>++ writes back';
}

# no match: nothing changes
{
    my @a = 1..6;
    for @a.grep(* > 100) { $_++ }
    is-deeply @a, [1, 2, 3, 4, 5, 6], 'empty grep result leaves array unchanged';
}

# assignment to a fresh array decontainerizes: @a must stay unchanged
{
    my @a = 1..6;
    my @g = @a.grep(* %% 2);
    for @g { $_++ }
    is-deeply @a, [1, 2, 3, 4, 5, 6], 'assigned grep copy does not write back to source';
    is-deeply @g, [3, 5, 7], 'assigned grep copy is itself mutated';
}

# plain `for @a` writeback unaffected
{
    my @a = 1..4;
    for @a { $_++ }
    is-deeply @a, [2, 3, 4, 5], 'plain for @a writeback still works';
}

# single-element grep view
{
    my @a = 1..6;
    for @a.grep(* == 4) { $_++ }
    is-deeply @a, [1, 2, 3, 5, 5, 6], 'single-match grep writes back';
}
