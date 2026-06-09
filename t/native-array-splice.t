use Test;

# VM-native `splice` on a plain, untyped `@`-array (ledger §1: native receiver
# dispatch -> VM-native). The native path handles the simple, non-erroring
# forms (plain non-negative Int offset/count, non-lazy replacements); richer
# forms fall through to the interpreter. These assertions match `raku`.

plan 28;

# --- removed-list return value + in-place mutation -----------------------
{
    my @a = 1, 2, 3, 4, 5;
    is-deeply @a.splice(1, 2), [2, 3], 'splice(start, count) returns removed';
    is-deeply @a, [1, 4, 5], '... and mutates the array in place';
}

# splice(start) removes through the end
{
    my @b = 1, 2, 3, 4, 5;
    is-deeply @b.splice(2), [3, 4, 5], 'splice(start) removes to end';
    is-deeply @b, [1, 2], '... leaving the prefix';
}

# splice() with no args removes everything
{
    my @c = 1, 2, 3, 4, 5;
    is-deeply @c.splice, [1, 2, 3, 4, 5], 'splice() removes all';
    is-deeply @c, [], '... leaving an empty array';
}

# --- replacement values --------------------------------------------------
{
    my @d = 1, 2, 3, 4, 5;
    is-deeply @d.splice(1, 2, 'x', 'y', 'z'), [2, 3], 'splice with scalar replacements: removed';
    is-deeply @d, [1, 'x', 'y', 'z', 4, 5], '... inserts the replacements';
}

# Array replacement is flattened
{
    my @f = 1, 2, 3, 4, 5;
    is-deeply @f.splice(1, 2, [10, 20]), [2, 3], 'array replacement: removed';
    is-deeply @f, [1, 10, 20, 4, 5], '... array replacement is flattened in';
}

# Insert without removal (count == 0)
{
    my @g = 1, 2, 3;
    is-deeply @g.splice(1, 0, 'mid'), [], 'splice(start, 0, x): removes nothing';
    is-deeply @g, [1, 'mid', 2, 3], '... inserts at the offset';
}

# Splice at the very end (start == elems)
{
    my @e = 1, 2, 3;
    is-deeply @e.splice(3, 0, 99), [], 'splice at end: removes nothing';
    is-deeply @e, [1, 2, 3, 99], '... appends the replacement';
}

# Count larger than the remaining elements is clamped
{
    my @h = 1, 2, 3;
    is-deeply @h.splice(1, 100), [2, 3], 'oversized count is clamped: removed';
    is-deeply @h, [1], '... leaving the unspliced prefix';
}

# --- emptiness / single element edge cases -------------------------------
{
    my @i = 42;
    is-deeply @i.splice(0, 1), [42], 'splice the only element';
    is-deeply @i, [], '... empties the array';
}
{
    my @j;
    is-deeply @j.splice, [], 'splice() on an empty array returns []';
    is-deeply @j, [], '... and stays empty';
}

# --- the spliced result is a fresh, independent array --------------------
{
    my @k = 1, 2, 3, 4;
    my @removed = @k.splice(1, 2);
    @removed.push(99);
    is-deeply @k, [1, 4], 'mutating the removed list does not touch the source';
    is-deeply @removed, [2, 3, 99], 'removed list is an independent array';
}

# --- interpreter-owned forms still work (fall-through) --------------------
# WhateverCode offset -> interpreter; native path returns None.
{
    my @l = 1, 2, 3, 4, 5;
    is-deeply @l.splice(*-2), [4, 5], 'WhateverCode offset falls through to interpreter';
    is-deeply @l, [1, 2, 3], '... and mutates correctly';
}

# Out-of-range offset throws X::OutOfRange (interpreter-owned).
{
    my @m = 1, 2, 3;
    dies-ok { @m.splice(10) }, 'offset past the end dies (X::OutOfRange)';
}

# Negative count throws (interpreter-owned).
{
    my @n = 1, 2, 3;
    dies-ok { @n.splice(0, -1) }, 'negative count dies (X::OutOfRange)';
}

# Splicing a lazy list in is X::Cannot::Lazy (interpreter-owned).
{
    my @o = 1, 2, 3;
    dies-ok { @o.splice(1, 1, (1..Inf).grep(* %% 2)) }, 'lazy replacement dies (X::Cannot::Lazy)';
}

# done in a loop: native writeback survives repeated splices
{
    my @p = 0 .. 9;
    @p.splice(0, 1) for ^5;
    is-deeply @p, [5, 6, 7, 8, 9], 'repeated splice in a loop mutates each time';
}
