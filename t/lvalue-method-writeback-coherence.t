use Test;

# Slice F (env<->locals coherence, docs/env-locals-coherence.md): the
# lvalue-method writeback builtins (`__mutsu_assign_method_lvalue` /
# `__mutsu_index_assign_method_lvalue`) mutate their target variable in `env`
# by name and historically relied on the reverse pull (`sync_locals_from_env`,
# gated by `env_dirty`) to refresh the caller's local slot before the next
# read. They now write the new value straight through to the local slot at the
# call site, so a subsequent read sees the mutation without the reverse-sync
# backstop. These cases must keep working (and, verified manually with
# `MUTSU_NO_REVERSE_SYNC=1`, work *without* the reverse pull).

plan 15;

# --- Pair .value lvalue ---
# The pair value must come from a *container* (`a => $v`): a Pair built over a
# literal has nothing to assign into and `.value = X` correctly raises
# X::Assignment::RO, as raku does.
{
    my $v = 5;
    my $p = a => $v;
    $p.value--;
    is $p.value, 4, 'Pair .value-- mutates and reads back coherently';
    $p.value = 10;
    is $p.value, 10, 'Pair .value = N reads back coherently';
    $p.value++;
    is $p.value, 11, 'Pair .value++ reads back coherently';
    is $v, 11, 'and the writes reached the source container';
}

# --- Array .head / .tail / .first lvalue ---
{
    my @a = 1, 2, 3;
    @a.head = 99;
    is @a[0], 99, '@a.head = v writes through and reads back';
    @a.tail = 77;
    is @a[*-1], 77, '@a.tail = v writes through and reads back';
}
{
    my @b = 10, 20, 30;
    @b.first(* > 15) = 999;
    is @b[1], 999, '@a.first(matcher) = v writes through and reads back';
}

# --- Hash AT-KEY lvalue ---
{
    my %h = a => 1, b => 2;
    %h.AT-KEY("a") = 42;
    is %h<a>, 42, '%h.AT-KEY(k) = v writes through and reads back';
}

# --- repeated mutation then immediate read (the reverse-pull-sensitive shape) ---
{
    my $w = 0;
    my $q = x => $w;
    $q.value = 1;
    is $q.value, 1, 'first .value = read-back';
    $q.value = 2;
    is $q.value, 2, 'second .value = read-back (no stale slot)';
    $q.value++;
    $q.value++;
    is $q.value, 4, 'chained .value++ read-back';
}

# --- mutation in a loop, read after each step ---
{
    my $n = 0;
    my $r = n => $n;
    my @log;
    for 1..3 {
        # A bare `$r.value` read hands out the element *cell* (that is what
        # `.VAR` needs), so `push` has to store a COPY of it -- otherwise every
        # element of @log would alias the final value. Fixed with ADR-0036
        # slice 3's `.pairs` routing; see
        # news/2026-09/pairs-hands-out-element-containers.md.
        $r.value++;
        @log.push($r.value);
    }
    is-deeply @log, [1, 2, 3], '.value++ in a loop reads the live value each iteration';
}

# --- index-assign method lvalue (nested container via accessor) ---
{
    my @grid = [1, 2], [3, 4];
    @grid.head[1] = 88;
    is @grid[0][1], 88, '@grid.head[i] = v writes through and reads back';
}

# --- for-loop topic `.value` writeback over a mutable QuantHash ---
# `.value = X for $b.pairs` mutates the source BagHash by name (via
# `quanthash_set_weight` with an empty CompiledCode); the for-loop writes the
# final env value of `$b` through to its local slot at loop end, so the
# post-loop read sees the new weight without the reverse pull.
{
    my $b = (a => 5).BagHash;
    .value = 999 for $b.pairs;
    is $b<a>, 999, '.value = N for $b.pairs writes the source weight back';
}
{
    my $bh = <a a b b b>.BagHash;
    for $bh.pairs { .value-- }
    is "$bh<a> $bh<b>", "1 2", '.value-- for $bh.pairs decrements each source weight';
}
