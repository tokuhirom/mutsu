# ADR-0049 §1.6/§4 slice 5: the multidim (`;`-separated) `:exists`/`:kv`/`:p`/
# `:delete` adverb family used to compute "does this leaf exist" with its own
# open-coded predicate (`!value.is_nil() && !matches!(Package("Any"))`) instead
# of the canonical `ArrayData::hole_at`. That predicate was blind to two
# things `hole_at` already knows: a typed array's own element-type gap marker
# (only the literal `"Any"` name was recognized), and the `initialized` set
# (so an explicitly-assigned `Any`/type-object value was indistinguishable
# from a genuine gap). Fixing it required threading a per-leaf hole flag
# through `multidim_collect_leaves`, AND a companion write-side fix: multidim
# element assignment (`@a[i;j] = v`, both the shaped and the autoviv/non-shaped
# form) never recorded the write in `ArrayData::initialized` at all, so even a
# perfectly precise reader had no accurate data to consult. `:delete` had the
# matching gap on removal.
#
# raku itself does not support `:exists`/`:kv`/`:p`/`:delete` on a multidim
# subscript when any dimension is `Whatever` (`*`) or a list of indices --
# `raku -e 'my @a[2;2]; say @a[*;*]:exists'` dies with ":exists on
# multi-dimensional slices not yet implemented. Sorry." Every assertion below
# that exercises that combination is therefore a mutsu-only, self-consistency
# check (cross-checked against the plain-coordinate form of the same query,
# which IS raku-comparable and is pinned first in each block) rather than a
# raku-comparable one; those blocks say so explicitly. Everything else in this
# file passes identically under `raku` and `mutsu`.

use v6;
use Test;

plan 28;

# --- Control: the plain (single-dimension, non-multidim) `:exists` path was
# already correct before this fix and must stay that way. ------------------
{
    my Int @a;
    @a[2] = 5;
    is-deeply (@a[0]:exists), False, 'control: single-dim typed gap does not exist';

    my @b;
    @b[0] = 1;
    @b[1] = Any;
    is-deeply (@b[1]:exists), True, 'control: single-dim explicit Any is not a hole';
}

# --- Multidim, plain coordinates, untyped shaped array: an unwritten slot is
# a hole; an explicitly-assigned `Any` is not. Raku-comparable. ------------
{
    my @a[2;2];
    @a[0;0] = 1;
    is-deeply (@a[0;1]:exists), False, 'shaped multidim: unwritten slot does not exist';
    is-deeply (@a[0;1]:kv), (), 'shaped multidim: :kv on a hole is empty';

    @a[0;1] = Any;
    is-deeply (@a[0;1]:exists), True,
        'shaped multidim: explicitly-assigned Any is not a hole';
    is-deeply (@a[0;1]:kv), ((0,1), Any),
        'shaped multidim: :kv on an explicitly-assigned Any is a real pair';
}

# --- Multidim, plain coordinates, autoviv (non-shaped) array: same rule. --
{
    my @a;
    @a[0;1] = 5;
    is-deeply (@a[0;0]:exists), False, 'autoviv multidim: unwritten sibling does not exist';

    @a[0;0] = Any;
    is-deeply (@a[0;0]:exists), True,
        'autoviv multidim: explicitly-assigned Any is not a hole';
}

# --- Multidim `:delete`, plain coordinates: raku-comparable. --------------
{
    my @a[2;2];
    @a[0;0] = 1;
    @a[0;1] = 99;
    is-deeply (@a[0;1]:delete), 99, 'shaped multidim :delete returns the value';
    is-deeply (@a[0;1]:exists), False, 'shaped multidim: deleted slot does not exist';

    my @b;
    @b[0;1] = 5;
    is-deeply (@b[0;1]:delete), 5, 'autoviv multidim :delete returns the value';
    is-deeply (@b[0;1]:exists), False, 'autoviv multidim: deleted slot does not exist';
}

# --- Multidim Whatever `:exists`/`:kv`/`:p` -- MUTSU-ONLY (raku throws
# ":exists on multi-dimensional slices not yet implemented" for this
# combination). Cross-checked for self-consistency against the plain-
# coordinate assertions above, which ARE raku-comparable. ------------------
{
    my @a[2;2];
    @a[0;0] = 1;
    @a[0;1] = Any;
    # Row 0: [0;0] a real value, [0;1] an explicit Any (exists). Row 1: both
    # unwritten holes. Matches the per-coordinate results pinned above.
    is-deeply (@a[*;*]:exists), (True, True, False, False),
        'shaped multidim Whatever :exists agrees with the per-coordinate form';
    is-deeply (@a[*;*]:kv), ((0,0), 1, (0,1), Any),
        'shaped multidim Whatever :kv keeps only the two real leaves';
    is-deeply (@a[*;*]:p), ((0,0) => 1, (0,1) => Any),
        'shaped multidim Whatever :p keeps only the two real leaves';
    is-deeply (@a[*;*]:!exists), (False, False, True, True),
        'shaped multidim Whatever :!exists negates the same predicate';
}

# --- Multidim list-index `:exists` -- also MUTSU-ONLY (same raku
# limitation; `has_multi_indices` treats a list index the same as Whatever).
{
    my @a[2;2];
    @a[0;0] = 1;
    @a[0;1] = Any;
    is-deeply (@a[0;(0,1)]:exists), (True, True),
        'shaped multidim list-index: row 0 (real value + explicit Any) both exist';
    is-deeply (@a[1;(0,1)]:exists), (False, False),
        'shaped multidim list-index: row 1 (both unwritten) neither exists';
}

# --- Multidim Whatever `:exists` on the autoviv (non-shaped) form -- also
# mutsu-only, cross-checked against the raku-comparable plain-coordinate
# assertions above. ---------------------------------------------------------
{
    my @a;
    @a[0;1] = 5;
    @a[0;0] = Any;
    is-deeply (@a[*;*]:exists), (True, True),
        'autoviv multidim Whatever :exists agrees with the per-coordinate form';
}

# --- Multidim Whatever `:delete` -- mutsu-only; verifies the deletion
# itself is reflected in a subsequent plain-coordinate (raku-comparable)
# :exists read, and that a not-yet-written sibling in the same row is
# unaffected. ----------------------------------------------------------------
{
    my @a[2;2];
    @a[0;0] = 1;
    @a[0;1] = 2;
    is-deeply (@a[0;*]:delete), (1, 2), 'shaped multidim Whatever :delete returns both values';
    is-deeply (@a[0;0]:exists), False,
        'shaped multidim Whatever :delete: first deleted slot does not exist';
    is-deeply (@a[0;1]:exists), False,
        'shaped multidim Whatever :delete: second deleted slot does not exist';
    is-deeply (@a[1;0]:exists), False,
        'shaped multidim Whatever :delete: untouched sibling row is unaffected (still a hole)';
}

# --- The dynamic (`:$delete`) adverb forms go through separate handlers
# (`__mutsu_multidim_exists_adverb_dyn` / `__mutsu_multidim_subscript_adverb_dyn`)
# with their own single-coordinate fallback; pin both. ---------------------
{
    my @a[2;2];
    @a[0;0] = 1;
    @a[0;1] = Any;
    my $no-delete = False;
    is-deeply (@a[0;1]:exists:delete($no-delete)), True,
        'dynamic :exists:delete(False): explicit Any still exists, plain coordinate';
    is-deeply (@a[0;1]), Any, 'dynamic :exists:delete(False) did not delete';

    my @b;
    @b[0;1] = 5;
    is-deeply (@b[0;0]:exists:delete($no-delete)), False,
        'dynamic :exists:delete(False): autoviv gap does not exist, plain coordinate';
}

# --- Nested Hash multidim `:exists` is unaffected by this fix (a missing
# Hash key is already precisely represented by `Value::NIL`, never consulting
# `ArrayData::hole_at`) -- control, raku-comparable. ------------------------
{
    my %h;
    %h{"a";"b"} = 5;
    is-deeply (%h{"a";"b"}:exists), True, 'hash multidim: existing compound key exists';
    is-deeply (%h{"a";"c"}:exists), False, 'hash multidim: missing compound key does not exist';
}

# vim: expandtab shiftwidth=4
