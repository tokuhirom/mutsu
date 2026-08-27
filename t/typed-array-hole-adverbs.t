use v6;
use Test;

plan 28 + 26;

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

# --- Multidim (`;`-separated) twin of the above -----------------------------
#
# `todo/tickets/multidim-value-adverb-hole-returns-nil-not-empty-list.md`:
# `@a[i;j]:v` (and `:k`/`:p`/`:kv`) on a multidim hole used to return `Nil`
# instead of the `()` every single-dimension case above already gets right.
#
# Fixing that surfaced a second, larger divergence in the SAME handlers: the
# negated forms (`:!v`/`:!k`/`:!p`/`:!kv`) were also wrong for a *filled*
# multidim slot, not just a hole (`@a[0;0]:!k` answered `Nil` where raku
# answers the value `500`). Verified directly against `raku`: unlike the
# single-dimension form just above (where `:!k` on a hole keeps the index,
# see "typed array: :!k on a hole is the index"), real Rakudo's multidim
# `[;]` postcircumfix collapses ALL FOUR negated adverbs to plain value
# access -- `@a[i;j]:!k`, `:!p`, and `:!kv` all answer the exact same raw
# value `:!v` would, both for a filled slot and for a hole. This is a
# genuine Rakudo multidim quirk (confirmed with `raku`, not a mutsu
# assumption), so the test pins mutsu's collapsed shape deliberately.
{
    my @a[2;2];
    @a[0;0] = 500;

    is-deeply (@a[0;1]:exists), False, 'multidim: unwritten slot does not exist';
    is-deeply (@a[0;0]:exists), True, 'multidim: assigned slot exists';

    is-deeply (@a[0;1]:v), (), 'multidim: :v on a hole is empty';
    is-deeply (@a[0;0]:v), 500, 'multidim: :v on an assigned slot is the value';
    is-deeply (@a[0;1]:k), (), 'multidim: :k on a hole is empty';
    is-deeply (@a[0;0]:k), (0, 0), 'multidim: :k on an assigned slot is the key tuple';
    is-deeply (@a[0;1]:p), (), 'multidim: :p on a hole is empty';
    is-deeply (@a[0;0]:p), ((0, 0) => 500), 'multidim: :p on an assigned slot is the pair';
    is-deeply (@a[0;1]:kv), (), 'multidim: :kv on a hole is empty';
    is-deeply (@a[0;0]:kv), ((0, 0), 500), 'multidim: :kv on an assigned slot is (key, value)';

    # Negated forms: collapse to plain value access, hole or not, regardless
    # of which of :!v/:!k/:!p/:!kv was requested.
    is-deeply (@a[0;1]:!v), Any, 'multidim: :!v on a hole is Any';
    is-deeply (@a[0;0]:!v), 500, 'multidim: :!v on an assigned slot is the value';
    is-deeply (@a[0;1]:!k), Any, 'multidim: :!k on a hole is Any, NOT the key';
    is-deeply (@a[0;0]:!k), 500, 'multidim: :!k on an assigned slot is the value, NOT the key';
    is-deeply (@a[0;1]:!p), Any, 'multidim: :!p on a hole is Any, NOT a pair';
    is-deeply (@a[0;0]:!p), 500, 'multidim: :!p on an assigned slot is the value, NOT a pair';
    is-deeply (@a[0;1]:!kv), Any, 'multidim: :!kv on a hole is Any, NOT a (key, value) list';
    is-deeply (@a[0;0]:!kv), 500, 'multidim: :!kv on an assigned slot is the value, NOT a list';
}

# A shaped multidim array's own fixed dimensions are just bounds, not
# elements: a coordinate outside them is genuinely missing. Unlike an
# in-bounds hole (which has its own non-Nil hole marker and reports `()`),
# an out-of-range coordinate has no marker of its own -- it is a bare `Nil`,
# the same representation a missing Hash key has -- so `:v`/`:k`/`:p` report
# `Nil`, not `()`, matching `roast/S32-array/multislice-6e.t` and
# `roast/S32-hash/multislice-6e.t` (both under `use v6.e.PREVIEW`; mutsu does
# not branch this behavior on the language-version pragma, so it applies the
# same rule under the default version too -- see
# `todo/tickets/multidim-oob-coordinate-nil-vs-empty-list-version-pragma.md`
# for the narrow divergence that leaves against plain, non-PREVIEW `raku`).
{
    my @a[2;2];
    @a[0;0] = 1;
    is-deeply (@a[5;5]:v), Nil, 'multidim: :v on an out-of-range coordinate is Nil';
    is-deeply (@a[5;5]:k), Nil, 'multidim: :k on an out-of-range coordinate is Nil';
    is-deeply (@a[5;5]:exists), False, 'multidim: out-of-range coordinate does not exist';
}

# An autoviv (non-shaped) multidim array follows the same rules as the shaped
# one above.
{
    my @c;
    @c[0;1] = 5;
    is-deeply (@c[0;0]:exists), False, 'autoviv multidim: unwritten sibling does not exist';
    is-deeply (@c[0;1]:exists), True, 'autoviv multidim: assigned slot exists';
    is-deeply (@c[0;0]:v), (), 'autoviv multidim: :v on a hole is empty';
    is-deeply (@c[0;1]:v), 5, 'autoviv multidim: :v on an assigned slot is the value';
    is-deeply (@c[0;0]:!v), Any, 'autoviv multidim: :!v on a hole is Any';
}
