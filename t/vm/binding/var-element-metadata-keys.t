use v6;
use Test;

# The second half of the `__mutsu_<ns>::<name>` env-key families, and the
# companion to `t/vm/binding/var-metadata-key-namespaces.t`. That file covers
# the namespaces that describe a WHOLE binding (is it a sigilless alias, is it
# `:=`-bound, what type constrains it); this one covers the rest, which issue
# #8087 stage 3 moved onto `MetaNs` (src/runtime/meta_ns.rs): metadata about
# individual ELEMENTS of a container, and about a name's relationship to a
# surrounding scope.
#
# The failure mode these guard against is silent in both directions:
#
#   * a mistyped prefix makes the writer and the reader disagree, and the
#     metadata is simply never found -- no error, just a `:delete`d hole that
#     fills back in, or a readonly element that accepts a write; and
#   * an env write that reaches `Env::insert_sym` instead of
#     `Env::insert_sym_noting` skips `note_env_key`, the latch that arms the
#     `elem_index_meta_possible()` / `bound_marker_possible()` fast-path probes.
#     The value is then stored correctly and the reader never looks, for the
#     rest of the process. Stage 2 of #8087 shipped exactly that bug and paid a
#     CI cycle for it, which is why these are behavioural tests and not
#     assertions about spelling.
#
# Several of these run on the element-store hot path (`@a[$i] = $v`), which is
# why they hold their key as a pre-interned `Symbol` and probe with `get_sym` /
# `get_mut_sym` / `remove_sym` rather than rebuilding a `String` per access.

plan 16;

# __mutsu_deleted_index:: -- the set of indices `:delete`d out of a container.
# An array hole reads as missing even though the slot holds a type object, so
# the marker is the only thing separating "deleted" from "never assigned".
{
    my @a = 1, 2, 3;
    @a[1]:delete;
    nok @a[1]:exists, 'a deleted element does not exist';
    is @a.elems, 3, 'but the array keeps its length';
    @a[1] = 9;
    ok @a[1]:exists, 'assigning into the hole clears the deleted marker';
    is @a[1], 9, 'and the new value is readable';
}

# __mutsu_ro_index:: -- which indices are readonly. `@a[0] := $x` where the
# source is a constant makes that one element refuse assignment while its
# siblings stay writable.
{
    my @a = 1, 2, 3;
    my $ro = 7;
    @a[0] := $ro;
    @a[1] = 20;
    is @a[1], 20, 'a sibling element is still writable';
    is @a[0], 7, 'the bound element reads its binding';
}

# __mutsu_elem_share:: -- an element holding a shared cell that an element store
# must write THROUGH, as against replacing the slot. `@aoa[0] = @row` shares;
# a later `@aoa[0] = 42` must replace instead.
{
    my @row = 1, 2;
    my @aoa;
    @aoa[0] = @row;
    @aoa[0].push(3);
    is @row.elems, 3, 'a shared element writes through to the source array';
    @aoa[0] = 42;
    is @aoa[0], 42, 'a later scalar assignment replaces the slot instead';
    is @row.elems, 3, 'and does not reach the source array';
}

# __mutsu_array_share:: -- the scalar end of the same mechanism. `$n = @z`
# itemizes the array into the scalar; pushing through the scalar is visible in
# the array, but reassigning the scalar is not.
{
    my @z = 1, 2;
    my $n = @z;
    $n.push(3);
    is @z.elems, 3, 'a push through the itemized scalar reaches the array';
    $n = 'other';
    is @z.elems, 3, 'reassigning the scalar replaces its slot, leaving the array alone';
}

# __mutsu_constant_var:: -- `constant` is a compile-time value, not merely a
# readonly binding, and the two are distinguished by this key alone.
{
    constant LIMIT = 10;
    is LIMIT, 10, 'a constant reads back';
    dies-ok { EVAL 'constant K = 1; K = 2' }, 'and refuses assignment';
}

# __mutsu_outer:: -- the enclosing scope's value of a name, snapped at closure
# capture so `$OUTER::` still reaches it after an inner declaration shadows it.
{
    my $x = 'outer';
    my $got = do { my $x = 'inner'; $OUTER::x };
    is $got, 'outer', '$OUTER:: sees the shadowed enclosing binding';
}

# __mutsu_gather_self_ref:: -- the aggregate a `gather` is being assigned to.
# A self-mention inside the block must not reify the half-built sequence.
{
    my @acc = gather {
        take 1;
        take 2;
    };
    is @acc.join(','), '1,2', 'a gather assigned to an array collects its takes';
}

# __mutsu_deep_readonly:: -- a binding that refuses method-based mutation too,
# not just assignment. A readonly `Pair` must reject `.value = ...`.
{
    sub take-pair($p) { $p }
    my $p = take-pair((a => 1));
    dies-ok { $p.value = 2 }, 'a readonly parameter refuses mutation through a method';
}
