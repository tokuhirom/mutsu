use Test;

# `ArrayData::initialized` is the canonical hole predicate (ADR-0049 §1.6): an
# array that never tracked gaps carries `None` ("bulk-constructed, no gaps") and
# one that does carries the set of indices actually written. The `;`-separated
# multidim path learned to seed and mark it in
# `news/2026-08/multidim-exists-adverb-canonical-hole-predicate.md`.
#
# The CHAINED-bracket form (`@a[i][j] = v`) compiles to entirely different
# opcodes -- `IndexAssignExprNested` and `IndexAssignDeepNested`, not
# `MultiDimIndexAssign` -- so none of those sites ran for it. Every array it
# autovivified was born untracked, and every slot the write never touched
# reported `:exists == True`.
#
# Every expectation below was measured against raku v2026.07.

plan 14;

# The headline: a sibling slot of the written one is a hole, at both levels.
{
    my @a;
    @a[0][1] = 5;
    nok @a[0][0]:exists, 'an unwritten sibling of a chained write is a hole';
    ok  @a[0][1]:exists, 'and the written slot exists';
}

# The outer level tracks gaps too, not just the freshly-created row.
{
    my @d;
    @d[1][1] = 5;
    nok @d[0]:exists, 'an unwritten outer slot is a hole';
    ok  @d[1]:exists, 'and the written outer slot exists';
}

# Three levels go through `IndexAssignDeepNested`, a different opcode again.
{
    my @e;
    @e[0][1][2] = 5;
    nok @e[0][0]:exists,    'a hole two levels down the deep-nested walk';
    nok @e[0][1][1]:exists, 'and a hole beside the deep-nested leaf';
    ok  @e[0][1][2]:exists, 'while the leaf itself exists';
}

# A hash root vivifying an array row.
{
    my %h;
    %h<a>[1] = 5;
    nok %h<a>[0]:exists, 'a hole in an array row vivified under a hash key';
}

# What must NOT change: a bulk-constructed array has no gaps, and filling a
# hole in afterwards makes it exist.
{
    my @p = 1, 2, 3;
    ok @p[0]:exists, 'a bulk-constructed array reports no gaps';

    my @r;
    @r[0][1] = 5;
    @r[0][0] = 7;
    ok @r[0][0]:exists, 'writing the hole afterwards fills it';

    my @u;
    @u[1][1] = 5;
    @u[0] = [9];
    ok @u[0]:exists, 'writing the outer hole afterwards fills it';

    my @w;
    @w[0][1] = 5;
    @w[0].push(8);
    ok @w[0][2]:exists, 'a push onto a vivified row appends a real element';

    my @x;
    @x[0][1] = 5;
    @x[0][1]:delete;
    nok @x[0][1]:exists, ':delete on a vivified row turns the slot back into a hole';
}

# The stored shape is unchanged -- this is about the gap bookkeeping, not the
# values.
{
    my @z;
    @z[0][1] = 5;
    is @z.raku, '[[Any, 5],]', 'the vivified structure itself is unchanged';
}
