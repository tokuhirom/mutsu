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

plan 12;

# --- Pair .value lvalue ---
{
    my $p = a => 5;
    $p.value--;
    is $p.value, 4, 'Pair .value-- mutates and reads back coherently';
    $p.value = 10;
    is $p.value, 10, 'Pair .value = N reads back coherently';
    $p.value++;
    is $p.value, 11, 'Pair .value++ reads back coherently';
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
    my $q = x => 0;
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
    my $r = n => 0;
    my @log;
    for 1..3 {
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
