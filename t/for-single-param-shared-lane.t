use Test;

plan 4;

# Activate the cross-thread bare-name store before either loop. A named loop
# parameter is a fresh binding and must stay off that store even for a container
# sigil: otherwise each iteration pulls the previous binding back over its item.
await start { 1 };

{
    my @matrix = [1, 2], [3, 4];
    for @matrix -> @row { @row.push(9) }
    is-deeply @matrix, [[1, 2, 9], [3, 4, 9]],
        'an @-sigil loop parameter rebinds to every row after start';
}

{
    my @matrix = [5, 6], [7, 8];
    for @matrix -> @row { @row.push(0) }
    is-deeply @matrix, [[5, 6, 0], [7, 8, 0]],
        'a later independent @-sigil loop does not reuse a stale binding';
}

{
    my @maps = { a => 1 }, { b => 2 };
    for @maps -> %row { %row<c> = 3 }
    is-deeply @maps, [{ a => 1, c => 3 }, { b => 2, c => 3 }],
        'a %-sigil loop parameter rebinds to every hash after start';
}

# Masking the name lane must not interfere with lexical capture. A start block
# captures the iteration binding through its ContainerRef cell.
{
    my @got = await do for 10, 20 -> $value { start { $value } };
    is-deeply @got, [10, 20],
        'a start block still captures a single named loop parameter';
}
