use v6;
use Test;

plan 28;

# A typed array's unassigned slot is seeded with its *element type* object, not
# `Any`. `:exists` has always known that; the value adverbs (`:v`/`:k`/`:p`/`:kv`)
# used to open-code a weaker predicate that only recognised `Any`, so an
# `Int`-seeded gap read back as a real `Int` value. Both now share
# `ArrayData::hole_at`.

{
    my Int @j;
    @j[2] = 5;

    is-deeply (@j[0]:exists), False, 'typed array: unassigned slot does not exist';
    is-deeply (@j[0]:v), (), 'typed array: :v on a hole is empty';
    is-deeply (@j[0]:k), (), 'typed array: :k on a hole is empty';
    is-deeply (@j[0]:p), (), 'typed array: :p on a hole is empty';
    is-deeply (@j[0]:kv), (), 'typed array: :kv on a hole is empty';

    is-deeply (@j[2]:exists), True, 'typed array: assigned slot exists';
    is-deeply (@j[2]:v), 5, 'typed array: :v on an assigned slot is the value';
    is-deeply (@j[2]:k), 2, 'typed array: :k on an assigned slot is the index';
}

# The negated forms keep the missing slots, reporting the element type as the
# hole marker.
{
    my Int @j;
    @j[2] = 5;

    is-deeply (@j[0]:!v), Int, 'typed array: :!v on a hole is the element type';
    is-deeply (@j[0]:!k), 0, 'typed array: :!k on a hole is the index';
}

# Slices drop the holes and keep only the assigned slots.
{
    my Int @j;
    @j[2] = 5;

    is-deeply (@j[0, 1, 2]:v), (5,), 'typed array slice: :v keeps only real values';
    is-deeply (@j[0, 1, 2]:k), (2,), 'typed array slice: :k keeps only real indices';
    is-deeply (@j[0, 1, 2]:kv), (2, 5), 'typed array slice: :kv keeps only real pairs';
    is-deeply (@j[0, 1, 2]:exists), (False, False, True),
        'typed array slice: :exists agrees with the value adverbs';
}

# Same for a Str-typed array, and for a gap below an explicitly assigned index.
{
    my Str @s;
    @s[3] = "x";

    is-deeply (@s[1]:exists), False, 'Str array: unassigned slot does not exist';
    is-deeply (@s[1]:v), (), 'Str array: :v on a hole is empty';
    is-deeply (@s[1]:kv), (), 'Str array: :kv on a hole is empty';
    is-deeply (@s[3]:v), "x", 'Str array: :v on an assigned slot is the value';
}

# A shaped typed array is pre-filled with its element type too; being fixed-size
# says how many slots exist, not that anything was written to them.
{
    my Int @i[3];

    is-deeply (@i[0]:exists), False, 'shaped typed array: unwritten slot does not exist';
    is-deeply (@i[0]:v), (), 'shaped typed array: :v on a hole is empty';
    is-deeply (@i[0]:k), (), 'shaped typed array: :k on a hole is empty';

    @i[1] = 9;
    is-deeply (@i[1]:v), 9, 'shaped typed array: :v after assignment is the value';
    is-deeply (@i[0, 1, 2]:v), (9,), 'shaped typed array slice: :v keeps only real values';
}

# An untyped array is unaffected: a real `Any` element is a value, an
# autovivification gap is not.
{
    my @a;
    @a[2] = 5;
    is-deeply (@a[0]:v), (), 'untyped array: :v on an autoviv gap is empty';

    my @n = 1, Nil, 3;
    is-deeply (@n[1]:exists), True, 'untyped array: an assigned Nil is a real element';
    is-deeply (@n[1]:v), Any, 'untyped array: an assigned Nil reads back as Any';
}

# `:delete` leaves a hole that both predicates agree on.
{
    my @d = 1, 2, 3;
    @d[1]:delete;
    is-deeply (@d[1]:exists), False, 'deleted slot does not exist';
    is-deeply (@d[1]:v), (), ':v on a deleted slot is empty';
}
