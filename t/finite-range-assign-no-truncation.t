use Test;

# Regression pin for `todo/tickets/finite-range-assign-truncates-at-100k.md`:
# assigning a *finite* Range of more than 100_000 elements to an `@` variable
# (or via slurpy `*@a` binding, or slice-assignment) used to silently
# truncate to 100_000 elements. The cap (now `MAX_LAZY_RANGE_PREFIX` in
# `src/runtime/utils.rs`) is only supposed to apply to *infinite* ranges
# (`b == i64::MAX`, e.g. `^Inf`, `1..*`) so that binding one into a `Lazy`
# array doesn't loop forever -- a finite range has a real bound and must
# always expand to it in full, matching raku.

plan 14;

# --- the five diverging repros from the ticket ---

{
    my @a = ^300_000;
    is @a.elems, 300_000, '^300_000 assigned to @ does not truncate .elems';
}

{
    my @a = ^300_000;
    is @a[299_999], 299_999, '^300_000 assigned to @ does not truncate the last element';
}

{
    my @a = 1..300_000;
    is @a.elems, 300_000, '1..300_000 assigned to @ does not truncate';
}

{
    sub s(*@x) { @x.elems }
    is s(1..300_000), 300_000, 'slurpy *@x binding a finite range does not truncate';
}

{
    my @c;
    @c[0..299_999] = 1..300_000;
    is @c.elems, 300_000, 'slice-assign of a finite range to @c[0..299_999] does not truncate';
}

# --- the four correct controls (must remain correct) ---

{
    is (my @d = (^300_000).List).elems, 300_000, '(^300_000).List assigned to @ stays correct';
}

{
    is (my @e = (^300_000).Array).elems, 300_000, '(^300_000).Array assigned to @ stays correct';
}

{
    is (1..300_000).elems, 300_000, 'a bare finite Range .elems stays correct';
}

{
    my $n = 0;
    $n++ for ^300_000;
    is $n, 300_000, 'for-loop over a finite range stays correct';
}

# --- infinite ranges must still be lazily capped, not eagerly expanded ---

{
    my @a = ^Inf;
    is @a[5], 5, '^Inf assigned to @ stays lazily indexable';
}

{
    my @a = 1..Inf;
    is @a[5], 6, '1..Inf assigned to @ stays lazily indexable';
}

{
    sub s(*@x) { @x[5] }
    is s(1..*), 6, 'slurpy *@x binding an infinite range stays lazily indexable';
}

# --- one case per sibling constant, above the old 100_000 cap ---

{
    # slurpy (MAX_SLURPY_RANGE_EXPAND, now unified into MAX_LAZY_RANGE_PREFIX)
    sub s(*@x) { @x.elems }
    is s(1..150_000), 150_000, 'slurpy *@x binding a >100k finite range does not truncate';
}

{
    # slice-assign (MAX_ASSIGN_SLICE_EXPAND, now unified into MAX_LAZY_RANGE_PREFIX)
    my @c;
    @c[0..149_999] = 1..150_000;
    is @c.elems, 150_000, 'slice-assign of a >100k finite range does not truncate';
}
