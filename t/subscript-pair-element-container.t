use Test;

# ADR-0036: a Pair produced by a subscript adverb (`:p`/`:kv`) or `.pairs`
# carries the element's live *Scalar container*, not a snapshot -- so reading
# through the pair/list later sees array/hash mutations, and writing through
# it (`.value = X`) writes the source container back, exactly like an ordinary
# `Array`/`Hash` element does in raku.
#
# This is the acceptance oracle for ADR-0036's phased rollout
# (docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md):
#
#   Slice 1 (this file)     -- pin every §1.3 row as an expectation, `todo`
#                               where the fix has not landed yet.
#   Slice 2 (landed)        -- `:p`/`:kv` subscript adverbs on a genuine
#                               mutable Array/Hash route through
#                               `array_slot_ref`/`hash_slot_ref`, and the
#                               `:kv` parser rewrite that used to make
#                               `(@a[0]:kv)[1] = x` work "by accident" for one
#                               syntactic shape only is deleted.
#   Slice 3 (not landed)    -- `.pairs`/`.kv`/`.antipairs` route through the
#                               same element-container mechanism at the VM
#                               method dispatch layer.
#   Slice 4 (not landed)    -- the promoted cell carries the container's
#                               element type constraint, and the
#                               `methods_mut_method_lvalue.rs` env-scan
#                               compensator is deleted.
#
# Every expected value below was cross-checked against `raku` (see the ADR's
# §1.3 table and this file's commit for the exact `raku -e` invocations).

plan 17;

# --- §1.3 row 1: :p stale read (Slice 2) -----------------------------------
{
    my @a = <A B>;
    my $p = @a[0]:p;
    @a[0] = "Q";
    is $p.value, "Q", ':p pair value tracks a later array write (row 1)';
}

# --- §1.3 row 2: :kv stale read (Slice 2) -----------------------------------
{
    my @a = <A B>;
    my $kv = @a[0]:kv;
    @a[0] = "Q";
    is-deeply $kv, (0, "Q"), ':kv list value tracks a later array write (row 2)';
}

# --- §1.3 row 3: .pairs stale read on an array (Slice 3) -------------------
{
    my @a = <A B>;
    my $p = @a.pairs[0];
    @a[0] = "Q";
    todo '.pairs element containers land in ADR-0036 slice 3';
    is $p.value, "Q", '.pairs pair value tracks a later array write (row 3)';
}

# --- §1.3 row 4: .pairs stale read on a hash (Slice 3) ----------------------
{
    my %h = a => 1;
    my $p = %h.pairs[0];
    %h<a> = 7;
    todo '.pairs element containers land in ADR-0036 slice 3';
    is $p.value, 7, '.pairs pair value tracks a later hash write (row 4)';
}

# --- §1.3 row 5: :p write-through, then .raku (Slice 2) ---------------------
{
    my @a = <A B>;
    my $p = @a[0]:p;
    $p.value = "x";
    is $p.raku, '0 => "x"', ':p pair.value = X writes through and re-reads it (row 5)';
    is-deeply @a, ["x", "B"], ':p pair.value = X wrote through to the source array';
}

# --- §1.3 row 6: .VAR.^name is Scalar for the promoted element (Slice 2) ---
{
    my @a = <A B>;
    is (@a[0]:p).value.VAR.^name, 'Scalar', ':p pair.value is a Scalar container (row 6)';
}
{
    my @a = <A B>;
    # Not an ADR-0036 row: `.VAR` special-cases only a *named*-variable index
    # target (`compile_expr_method_var_on_index`, src/compiler/expr.rs) to
    # report the element's container kind; an anonymous computed target like
    # `(@a[0]:kv)[1]` falls through to the general index-read chokepoint,
    # which decontainerizes before `.VAR` ever sees the cell. This is a
    # pre-existing `.VAR`-dispatch gap orthogonal to ADR-0036 (raku: `Scalar`
    # here too, via any array element read, not just a named one).
    todo '.VAR on an anonymous computed index target does not see a ContainerRef (pre-existing, outside ADR-0036)';
    is (@a[0]:kv)[1].VAR.^name, 'Scalar', ':kv list element is a Scalar container';
}
{
    my @a = <A B>;
    todo '.pairs element containers land in ADR-0036 slice 3';
    is @a.pairs[0].value.VAR.^name, 'Scalar', '.pairs pair.value is a Scalar container';
}

# --- §1.3 row 7: array ambiguity -- a sibling copy must not confuse :p -----
# (Slice 2 -- this is the defect the env-scan compensator could never fix:
# `my @b = @a` gives the old scan a second equal-valued candidate array, so it
# used to decline the write with a misleading X::Assignment::RO.)
{
    my @a = <A B>;
    my @b = @a;
    (@a[0]:p).value = "z";
    is-deeply @a, ["z", "B"], ':p writes through even with an equal sibling array in scope (row 7)';
}
{
    my @a = <A B>;
    my @b = @a;
    (@a[0]:kv)[1] = "z";
    is-deeply @a, ["z", "B"], ':kv[1] writes through even with an equal sibling array in scope';
}

# --- §1.3 row 8: hash ambiguity -- a sibling copy must not confuse :p ------
{
    my %h = a => 1;
    my %g = a => 1;
    (%h<a>:p).value = 9;
    is-deeply %h, {a => 9}, ':p writes through a hash even with an equal sibling hash in scope (row 8)';
}
{
    my %h = a => 1;
    my %g = a => 1;
    (%h<a>:kv)[1] = 9;
    is-deeply %h, {a => 9}, ':kv[1] writes through a hash even with an equal sibling hash in scope';
}

# --- §1.3 row 9: for @a.pairs -> $p { $p.value = ... } ambiguity (Slice 3) -
{
    my @a = <A B>;
    my @c = <A B>;
    for @a.pairs -> $p { $p.value = "y" }
    todo '.pairs element containers land in ADR-0036 slice 3';
    is-deeply @a, ["y", "y"], 'for @a.pairs writes through even with an equal sibling array (row 9)';
}

# --- §1.3 row 10: standalone `key => @a[elem]` pair ambiguity (Slice 3) ----
{
    my @a = <A B>;
    my $p = 0 => @a[0];
    my @c = <A B>;
    $p.value = "x";
    todo 'FatArrow container-capture over an Index RHS lands in ADR-0036 slice 3';
    is-deeply @a, ["x", "B"], 'key => @a[i] pair writes through with an equal sibling array (row 10)';
}

# --- §1.3 row 11: .pairs on an immutable List must die (Slice 3) ----------
{
    my $l = (1, 2);
    todo '.pairs element containers land in ADR-0036 slice 3';
    dies-ok { $l.pairs[0].value = 3 }, 'List.pairs[0].value = X dies, not a silent no-op (row 11)';
}

# --- §1.3 row 12: typed-array element constraint enforced on write (Slice 4)
{
    my Str @a = <A B>;
    todo 'element type constraint on the promoted cell lands in ADR-0036 slice 4';
    dies-ok { (@a[0]:p).value = 42 }, 'a typed array element constraint is enforced through :p (row 12)';
}
