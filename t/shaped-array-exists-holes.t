# A shaped array's slots start UNASSIGNED. Being fixed-size says how many slots
# there are, not that anything was written to them, so raku answers
# `my @a[3]; @a[0]:exists` with False until something is.
#
# mutsu answered True for every in-range index: the declaration allocated the
# array pre-filled with the element type object and left the embedded
# `initialized` set at `None` ("bulk-constructed, no gaps"), so the three
# `:exists` sites each carried an explicit shaped carve-out to stay consistent
# with each other. Seeding the empty set at allocation makes the ordinary hole
# predicate right, and the carve-outs are gone.
#
# Every assertion here also passes unmodified under rakudo.
use Test;
plan 32;

# --- an untyped shaped array ---
{
    my @t[3];
    is-deeply (@t[0]:exists), False, 'a fresh shaped slot does not exist';
    is-deeply @t.EXISTS-POS(0), False, 'and EXISTS-POS agrees';

    @t[1] = 5;
    is-deeply (@t[0]:exists), False, 'writing one slot does not fill the others';
    is-deeply (@t[1]:exists), True,  'the written slot exists';
    is-deeply (@t[2]:exists), False, 'and the one after it does not';
    is-deeply @t.EXISTS-POS(1), True,  'EXISTS-POS on the written slot';
    is-deeply @t.EXISTS-POS(2), False, 'EXISTS-POS on an untouched slot';
    is-deeply @t.EXISTS-POS(3), False, 'EXISTS-POS past the shape';

    is-deeply (@t[*]:exists).List, (False, True, False), 'a zen slice reports each slot';
    is-deeply (@t[0]:!exists), True,  ':!exists negates a hole';
    is-deeply (@t[1]:!exists), False, 'and a filled slot';
}

# --- a shaped array with an initializer has every slot assigned ---
{
    my @u[3] = 1, 2, 3;
    is-deeply (@u[0]:exists), True, 'an initialized shaped slot exists';
    is-deeply (@u[2]:exists), True, 'as does the last one';
    is-deeply @u.EXISTS-POS(1), True, 'and EXISTS-POS agrees';
}

# --- multidimensional ---
{
    my @m[2;2];
    is-deeply (@m[0;0]:exists), False, 'a fresh multidim cell does not exist';
    is-deeply @m.EXISTS-POS(0, 0), False, 'nor through EXISTS-POS';
    # The rows themselves are real containers the declaration built, so the
    # outer dimension is not a hole.
    is-deeply @m.EXISTS-POS(0), True, 'but the row it lives in does';

    @m[1;1] = 9;
    is-deeply (@m[1;1]:exists), True,  'the written cell exists';
    is-deeply (@m[0;0]:exists), False, 'and its neighbours still do not';

    my @n[2;2] = (1, 2), (3, 4);
    is-deeply (@n[0;0]:exists), True, 'an initialized multidim cell exists';
    is-deeply (@n[1;1]:exists), True, 'as does the last one';
}

# --- a typed shaped array seeds its cells with the element type object ---
{
    my Int @i[3];
    is-deeply (@i[0]:exists), False, 'an Int-seeded cell is still a hole';
    @i[2] = 4;
    is-deeply (@i[0]:exists), False, 'writing one cell does not fill the others';
    is-deeply (@i[2]:exists), True,  'and the written one exists';

    my Str @s[2];
    is-deeply (@s[0]:exists), False, 'the same for a Str-seeded cell';
    is-deeply @s.EXISTS-POS(1), False, 'through EXISTS-POS too';
}

# --- deleting a slot re-opens the hole ---
{
    my @d[3] = 1, 2, 3;
    @d[1]:delete;
    is-deeply (@d[1]:exists), False, 'a deleted shaped slot stops existing';
    is-deeply (@d[0]:exists), True,  'while its neighbour is untouched';
    is @d.raku, 'Array.new(:shape(3,), [1, Any, 3])', 'and the array keeps its shape';
}

# --- what must NOT change: an unshaped array ---
{
    my @a;
    @a[0] = 1;
    @a[2] = 3;
    is-deeply (@a[0]:exists), True,  'an unshaped autovivification gap still reports';
    is-deeply (@a[1]:exists), False, 'the gap itself does not exist';
    is-deeply (@a[2]:exists), True,  'and the assigned slot does';
}
