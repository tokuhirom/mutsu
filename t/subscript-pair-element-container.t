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
#   Slice 3 (landed)        -- the container-aware producer layer
#                               (src/vm/vm_element_producers.rs) carries
#                               ADR-0045 slice 4's `.values`/`.reverse`/`.sort`
#                               and, since 2026-09-01, `.pairs` itself -- see
#                               news/2026-09/pairs-hands-out-element-containers.md.
#                               `.antipairs` is deliberately NOT routed: it puts
#                               the element in the pair's KEY, and a pair key is
#                               never a container in raku (measured below).
#                               `.kv` joined them once a writable multi-parameter
#                               started binding raw (ADR-0045 slice 5) -- see
#                               news/2026-09/kv-hands-out-element-containers-to-a-multi-param-loop.md.
#   Slice 4 (landed)        -- the promoted cell carries the container's element
#                               type constraint (row 12), and the standalone-pair
#                               env rebind in `methods_mut_method_lvalue.rs` no
#                               longer fakes a write through an immutable pair
#                               value (row 11) -- see
#                               news/2026-09/pair-value-assign-enforces-immutability.md.
#
# Every expected value below was cross-checked against `raku` (see the ADR's
# §1.3 table and this file's commit for the exact `raku -e` invocations).

plan 39;

# --- §1.3 row 1: :p stale read (Slice 2) -----------------------------------
{
    my @a = <A B>;
    my $p = @a[0]:p;
    @a[0] = "Q";
    is $p.value, "Q", ':p pair value tracks a later array write (row 1)';
}

# A pair value is a first-class element cell. `.VAR` must preserve that it was
# explicitly reflected: the bare cell remains transparent, while its view
# reports the Scalar container even after a `:=` binding carries it onward.
{
    my @a = 10, 20;
    my $p = @a[0]:p;
    is $p.value.WHAT, Int, ':p pair.value WHAT decontainerizes the cell';
    is $p.value.VAR.^name, 'Scalar', ':p pair.value .VAR.^name sees Scalar';
    is $p.value.^name, 'Int', ':p pair.value bare .^name sees the value';
    is $p.value.VAR.WHAT, Scalar, ':p pair.value .VAR.WHAT sees Scalar';
    my $view := $p.value.VAR;
    is $view.WHAT, Scalar, 'a bound .VAR view keeps its Scalar identity';
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
    is $p.value, "Q", '.pairs pair value tracks a later array write (row 3)';
}

# --- §1.3 row 4: .pairs stale read on a hash (Slice 3) ----------------------
{
    my %h = a => 1;
    my $p = %h.pairs[0];
    %h<a> = 7;
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
    is-deeply @a, ["y", "y"], 'for @a.pairs writes through even with an equal sibling array (row 9)';
}

# --- §1.3 row 10: standalone `key => @a[elem]` pair ambiguity (Slice 3) ----
# Landed once `array_slot_ref` stopped vivifying eagerly: a FatArrow's `Index`
# RHS now compiles in the container-producing mode `=:=` / `return-rw` use, and
# an out-of-range index yields a deferred token rather than growing the array at
# pair-construction time.
{
    my @a = <A B>;
    my $p = 0 => @a[0];
    my @c = <A B>;
    $p.value = "x";
    is-deeply @a, ["x", "B"], 'key => @a[i] pair writes through with an equal sibling array (row 10)';
}
{
    # The out-of-range companion: constructing the pair must not grow `@a`,
    # and the write through `.value` fills the gap.
    my @a = 1, 2;
    my $p = 'k' => @a[5];
    is-deeply @a, [1, 2], 'key => @a[out-of-range] does not grow the array (row 10)';
    $p.value = 9;
    is @a.raku, '[1, 2, Any, Any, Any, 9]', 'the pair write vivifies the element (row 10)';
}

# --- §1.3 row 11: .pairs on an immutable List must die (Slice 4) ----------
# Slice 3 does the half it owns: a `List` receiver keeps the SNAPSHOT producer,
# so the pair value is a bare item with no container behind it — which is the
# whole of ADR-0036 §2.2's immutability story. The other half was the
# STANDALONE-PAIR ENV REBIND in `methods_mut_method_lvalue.rs` (not the
# env-scan, as this comment and the ADR originally said — instrumenting the
# path showed the scan never fires for a `List` receiver): with no backing
# container found it rebound any env binding holding an equal Pair and reported
# success. The read-only guard that sat next to it now covers every immutable
# scalar leaf rather than just `Bool`, so the write raises X::Assignment::RO.
{
    my $l = (1, 2);
    dies-ok { $l.pairs[0].value = 3 }, 'List.pairs[0].value = X dies, not a silent no-op (row 11)';
}

# --- §1.3 row 12: typed-array element constraint enforced on write (Slice 4)
{
    my Str @a = <A B>;
    # Green since 2026-09-01: the promoted element cell carries its array's
    # `value_type` (news/2026-09/is-rw-bare-tail-returns-container.md).
    dies-ok { (@a[0]:p).value = 42 }, 'a typed array element constraint is enforced through :p (row 12)';
    is-deeply @a, Array[Str].new("A", "B"), '... and the rejected write did not land (row 12)';
}

# --- Slice 3: the producers that now hand out element containers ------------
{
    my @a = <A B>;
    is @a.values[0].VAR.^name, 'Scalar', '.values hands out the element container';
}
{
    my @a = <A B>;
    is (@a.values)[0].WHAT.^name, 'Str', '.values index decontainerizes for .WHAT';
}
{
    my @a = <A B>;
    (@a.values)[0] = 'x';
    is-deeply @a, ['x', 'B'], 'a positional write through .values updates the source array';
}
{
    my @a = <A B>;
    (@a.kv)[1] = 'x';
    is-deeply @a, ['x', 'B'], 'a positional write through .kv updates the source array';
}
{
    my @a = <A B>;
    my $cell := (@a.values)[0];
    $cell = 'x';
    is-deeply @a, ['x', 'B'], 'a binding to a .values element updates the source array';
}
# The stale-READ direction, which the `.VAR` gap does not mask: a binding taken
# from `.values` must see a later write to the element.
{
    my @a = 10, 20;
    my @c;
    for @a.values -> $v is rw { @c.push(-> { $v }) }
    @a[0] = 5;
    is @c[0](), 5, 'a deferred read through a .values alias sees a later element write';
}
{
    my @a = 10, 20;
    for @a.values -> $v is rw { $v = $v + 1 }
    is-deeply @a, [11, 21], '.values -> $v is rw writes through';
}
{
    my @a = 10, 20;
    my @c;
    for @a.values -> $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [11, 21], 'an escaping closure over a .values alias writes through';
}

# `.antipairs` must NOT alias: the element sits in the pair's KEY, and a pair
# key is a value in raku. Measured: `$p.key` stays "A" after `@a[0] = "Q"`, and
# `$p.key.VAR.^name` is Str, not Scalar. ADR-0036 section 4 grouped it with
# `.pairs`/`.kv`; that grouping is corrected here.
{
    my @a = <A B>;
    my $p = @a.antipairs[0];
    @a[0] = "Q";
    is $p.key, 'A', '.antipairs key is a snapshot, not the element container';
}
{
    my @a = <A B>;
    is @a.antipairs[0].key.VAR.^name, 'Str', '.antipairs key is not a Scalar container';
}

# An immutable receiver keeps the snapshot producer, so its pair value is a bare
# item with nothing to alias -- ADR-0036 section 2.2's whole immutability story.
{
    my $l = (1, 2);
    is $l.pairs[0].value.VAR.^name, 'Int', 'a List keeps snapshot pair values';
}

# The promoted cells stay invisible to every ordinary read of the source.
{
    my @a = 10, 20, 30;
    @a.pairs.elems;
    @a.values.elems;
    @a.reverse.elems;
    @a.sort.elems;
    is @a.raku, '[10, 20, 30]', 'running every producer leaves .raku unchanged';
    is-deeply @a.List, (10, 20, 30), 'and list context still decontainerizes';
    is @a[0].WHAT.^name, 'Int', 'and an element still reads back as its own type';
}
