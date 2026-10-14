use Test;

# #13078: a push to a `my @a` that a `start` closure captured went through
# `Gc::make_mut`, which copied the array's node whenever another holder still
# referenced it (the worker's slot, a parameter bound to it, an element store's
# copy). The push landed on the copy and was lost, even with a single thread
# and no race. The write is now made in place under the cell's and the node's
# ADR-0068 stripes, as rakudo's VMArray grows in place.

plan 6;

{
    my @a = ^3;
    await start { @a[0] = 5; @a.push(1) };
    is-deeply @a, [5, 1, 2, 1], 'an element store then a push, inside one start';
}

{
    my @a = ^3;
    await start { @a[0] = 5; @a.push(1); @a[1] = 7 };
    is-deeply @a, [5, 7, 2, 1], 'a store after the push still sees it';
}

{
    my @a = ^3;
    await start { @a.push(1); @a[0] = 5; @a.push(2) };
    is-deeply @a, [5, 1, 2, 1, 2], 'two pushes around a store';
}

{
    my @a = ^3;
    await start { @a[0] = 5; @a.push(1); @a.push(2) };
    is-deeply @a, [5, 1, 2, 1, 2], 'two pushes after a store';
}

# The issue's shape: a routine that assigns into its `@x` parameter races
# other threads' pushes. Raku: every push survives.
{
    my @a = ^10;
    sub touch(@x) { @x[0] = @x[0] }
    await (^4).map: -> $i { start { for ^500 { $i %% 2 ?? touch(@a) !! @a.push(1) } } };
    is @a.elems, 10 + 2 * 500, 'a mutating callee loses no concurrent push';
}

{
    my @a = ^10;
    sub look(@x) { @x.elems }
    await (^4).map: -> $i { start { for ^500 { $i %% 2 ?? look(@a) !! @a.push(1) } } };
    is @a.elems, 10 + 2 * 500, 'a read-only callee loses no concurrent push';
}
