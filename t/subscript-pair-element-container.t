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
#   Slice 3 (partial)       -- the container-aware producer layer landed
#                               (src/vm/vm_element_producers.rs), and ADR-0045
#                               slice 4's `.values`/`.reverse`/`.sort` go
#                               through it. `.pairs` itself is DEFERRED: a Pair
#                               holding a cell leaks through the many consumers
#                               that destructure a pair's value as data --
#                               todo/tickets/pairs-element-containers-leak-through-pair-value-consumers.md.
#                               `.antipairs` is deliberately NOT routed: it puts
#                               the element in the pair's KEY, and a pair key is
#                               never a container in raku (measured below).
#                               `.kv` is deferred because its multi-parameter
#                               bind decontainerizes -- see
#                               todo/tickets/for-kv-multi-param-bind-decontainerizes.md.
#   Slice 4 (not landed)    -- the promoted cell carries the container's
#                               element type constraint, and the
#                               `methods_mut_method_lvalue.rs` env-scan
#                               compensator is deleted (rows 11 and 12).
#
# Every expected value below was cross-checked against `raku` (see the ADR's
# §1.3 table and this file's commit for the exact `raku -e` invocations).

plan 27;

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
    todo 'row 3 needs .pairs routed -- deferred, see todo/tickets/pairs-element-containers-leak-through-pair-value-consumers.md';
    is $p.value, "Q", '.pairs pair value tracks a later array write (row 3)';
}

# --- §1.3 row 4: .pairs stale read on a hash (Slice 3) ----------------------
{
    my %h = a => 1;
    my $p = %h.pairs[0];
    %h<a> = 7;
    todo 'row 4 needs .pairs routed -- deferred, see todo/tickets/pairs-element-containers-leak-through-pair-value-consumers.md';
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
    todo '.pairs routing deferred -- see todo/tickets/pairs-element-containers-leak-through-pair-value-consumers.md';
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
    todo 'row 9 needs .pairs routed -- deferred, see todo/tickets/pairs-element-containers-leak-through-pair-value-consumers.md';
    is-deeply @a, ["y", "y"], 'for @a.pairs writes through even with an equal sibling array (row 9)';
}

# --- §1.3 row 10: standalone `key => @a[elem]` pair ambiguity (Slice 3) ----
{
    my @a = <A B>;
    my $p = 0 => @a[0];
    my @c = <A B>;
    $p.value = "x";
    todo 'row 10 needs a non-vivifying array element token first -- todo/tickets/array-slot-ref-vivifies-eagerly-where-raku-defers.md';
    is-deeply @a, ["x", "B"], 'key => @a[i] pair writes through with an equal sibling array (row 10)';
}

# --- §1.3 row 11: .pairs on an immutable List must die (Slice 4) ----------
# Slice 3 does the half it owns: a `List` receiver keeps the SNAPSHOT producer,
# so the pair value is a bare item with no container behind it — which is the
# whole of ADR-0036 §2.2's immutability story. What still swallows the write is
# the OTHER half, the env-scan compensator in `methods_mut_method_lvalue.rs`:
# it finds `$l`'s own list as a candidate container, rebuilds it, and reports
# success. The read-only guard is only reachable once that scan is deleted,
# which is slice 4's job — so this row moves to slice 4 rather than slice 3.
{
    my $l = (1, 2);
    todo 'row 11 needs the env-scan compensator deleted (ADR-0036 slice 4)';
    dies-ok { $l.pairs[0].value = 3 }, 'List.pairs[0].value = X dies, not a silent no-op (row 11)';
}

# --- §1.3 row 12: typed-array element constraint enforced on write (Slice 4)
{
    my Str @a = <A B>;
    todo 'element type constraint on the promoted cell lands in ADR-0036 slice 4';
    dies-ok { (@a[0]:p).value = 42 }, 'a typed array element constraint is enforced through :p (row 12)';
}

# --- Slice 3: the producers that now hand out element containers ------------
{
    my @a = <A B>;
    # Same pre-existing `.VAR`-dispatch gap as line 88's `:kv` row: `.VAR` on an
    # ANONYMOUS computed index target (`@a.values[0]`) goes through the general
    # index-read chokepoint, which decontainerizes before `.VAR` ever sees the
    # cell. Not an ADR-0036 row -- see
    # todo/tickets/var-on-a-containerref-is-not-distinguishable.md.
    todo '.VAR on an anonymous computed index target does not see a ContainerRef (pre-existing)';
    is @a.values[0].VAR.^name, 'Scalar', '.values hands out the element container';
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
