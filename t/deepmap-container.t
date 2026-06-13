use v6;
use Test;

plan 16;

# deepmap passes each leaf as a container: a mutating callable writes
# through to the source structure (hyper.t 330-333 regression tests).

{
    my @a = (1, { a => 2, b => 3 }, 4);
    my @r = @a.deepmap(++*);
    is @r[0], 2, 'prefix ++ result: plain element';
    is @r[1]<a>, 3, 'prefix ++ result: hash value';
    is @r[2], 5, 'prefix ++ result: trailing element';
    is @a[0], 2, 'prefix ++ mutates source: plain element';
    is @a[1]<a>, 3, 'prefix ++ mutates source: hash value';
    is @a[1]<b>, 4, 'prefix ++ mutates source: hash value b';
    is @a[2], 5, 'prefix ++ mutates source: trailing element';
}

{
    my @a = (10, { x => 20 });
    my @r = @a.deepmap(*--);
    is @r[0], 10, 'postfix -- returns old value';
    is @r[1]<x>, 20, 'postfix -- returns old hash value';
    is @a[0], 9, 'postfix -- mutates source';
    is @a[1]<x>, 19, 'postfix -- mutates source hash value';
}

{
    # Nested arrays mutate through too.
    my @a = [1, [2, 3]];
    @a.deepmap(++*);
    is @a[0], 2, 'nested: top element mutated';
    is @a[1][0], 3, 'nested: inner element mutated';
}

{
    # Non-mutating blocks still work and do NOT touch the source.
    my @a = (1, { a => 2 }, 4);
    my @r = @a.deepmap(-*);
    is @r[0], -1, 'pure block result';
    is @a[0], 1, 'pure block leaves source untouched';
    # A block returning its argument must not leak a container.
    my @s = @a.deepmap({ $_ });
    is @s[0], 1, 'identity block returns plain value';
}
