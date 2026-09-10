use Test;

# An `is raw`/`is rw` parameter's own local slot must alias the CALLER's
# actual container, not a fresh, disconnected cell created independently at
# bind time. Regression for todo/tickets/is-raw-param-container-identity.md
# (ADR-0032 §2.1 probe `O`, explicitly out of scope for that ADR): a `key =>
# $x` (or other `WrapVarRef`-consuming) site inside the callee reads `$x`
# via `capture_var_cell_inner`'s "found a slot" branch, which was discarding
# the already-correct cell `bind_function_args_values` installed at `$x`'s
# own slot in favor of a by-name search for the CALLER's source variable
# name (an unrelated `__mutsu_sigilless_alias::` entry set up for a
# possible later `:=` through the param) -- that name is never a local of
# the callee's frame, so the search failed and a brand-new, disconnected
# cell was boxed instead.

plan 14;

# Probe O (ADR-0032 §2.1, verbatim): pointy block, `is raw`.
{
    my $v = 1;
    my $mk = -> $x is raw { key => $x };
    my $p = $mk($v);
    $v = 2;
    is $p.value, 2, 'O: pointy block is-raw param key=>$x aliases the caller container (read)';
}

# Same shape with `is rw`.
{
    my $v = 1;
    my $mk = -> $x is rw { key => $x };
    my $p = $mk($v);
    $v = 2;
    is $p.value, 2, 'is rw pointy-block param key=>$x aliases the caller container (read)';
}

# Write-through the other direction: mutating the Pair's value must reach
# the caller's variable.
{
    my $v = 1;
    my $mk = -> $x is raw { key => $x };
    my $p = $mk($v);
    $p.value = 9;
    is $v, 9, 'O (write-through): mutating $p.value writes back through $v';
}

# Named sub, `is raw` -- same bind path (`bind_function_args_values`), so it
# must share the fix.
{
    sub mk-raw($x is raw) { key => $x }
    my $v = 1;
    my $p = mk-raw($v);
    $v = 2;
    is $p.value, 2, 'named sub is-raw param key=>$x aliases the caller container';
}

# Named sub, `is rw`.
{
    sub mk-rw($x is rw) { key => $x }
    my $v = 1;
    my $p = mk-rw($v);
    $v = 2;
    is $p.value, 2, 'named sub is-rw param key=>$x aliases the caller container';
}

# Pair.new (the other WrapVarRef consumer besides fat-arrow) must alias too.
{
    my $mk = -> $x is raw { Pair.new("k", $x) };
    my $v = 1;
    my $p = $mk($v);
    $v = 2;
    is $p.value, 2, 'Pair.new(...,$x) inside an is-raw pointy block aliases the caller container';
}

# Surrounding-correctness controls: an `is raw`/`is rw` param that is NEVER
# captured through a WrapVarRef-consuming site (no Pair/Capture/List
# construction over it) must keep working exactly as before -- a plain
# read/write through the parameter still round-trips to the caller.
{
    sub bump-raw($x is raw) { $x = $x + 1; $x }
    my $v = 10;
    my $r = bump-raw($v);
    is $r, 11, 'never-captured is-raw param: return value reflects the write';
    is $v, 11, 'never-captured is-raw param: write reaches the caller variable';
}
{
    sub bump-rw($x is rw) { $x = $x + 1; $x }
    my $v = 10;
    my $r = bump-rw($v);
    is $r, 11, 'never-captured is-rw param: return value reflects the write';
    is $v, 11, 'never-captured is-rw param: write reaches the caller variable';
}

# A plain (non-raw, non-rw) parameter must NOT alias the caller: `key =>
# $x` on a normal by-value param snapshots the argument, and a later
# caller-side mutation must not leak into the already-built Pair.
{
    my $v = 1;
    my $mk = -> $x { key => $x };
    my $p = $mk($v);
    $v = 2;
    is $p.value, 1, 'plain (non-raw) param snapshots the argument -- no aliasing';
}

# A plain (non-rw, non-raw, non-copy) parameter is readonly in Raku, so
# mutating it must die rather than silently leaking (or silently succeeding)
# -- confirms the fix did not accidentally make plain params writable.
{
    sub bump-plain($x) { $x = $x + 1; $x }
    my $v = 10;
    dies-ok { bump-plain($v) }, 'plain param: assigning to it dies (readonly)';
    is $v, 10, 'plain param: the caller variable is unchanged after the dies-ok call';
}

# Two separate is-raw calls over the same caller variable must not cross-talk
# (each call gets its own fresh binding of the shared cell, not a stale one
# left over from a prior call).
{
    my $v = 1;
    my $mk = -> $x is raw { key => $x };
    my $p1 = $mk($v);
    $v = 2;
    my $p2 = $mk($v);
    $v = 3;
    is "{$p1.value} {$p2.value}", "3 3", 'two is-raw calls over the same variable both track the live container';
}
