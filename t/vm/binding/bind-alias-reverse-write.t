use v6;
use Test;

# Pin for news/2026-08/bind-alias-reverse-write-through-nested-cell.md:
# a `:=` bind performed INSIDE A SUB of a free/outer lexical must
# leave the alias a genuinely WRITABLE alias of the source -- a later write
# through the alias (`$alias = ...`) must reach the source variable too, not
# just the other direction (source write observed through the alias, which
# was already fixed and is pinned by t/bind-source-tracks-through-call-chain.t).
#
# Root cause: after the bind reuses the source's own cell, the alias's
# PRE-BIND storage cell (its ADR-0024 mainline/closure capture cell) merely
# CONTAINS the shared cell (a nested ContainerRef) instead of BEING it. A
# plain-value write through the alias's outer cell replaced the nested
# ContainerRef wholesale instead of writing through it, silently severing the
# alias link. Fixed by making `Value::store_through_cell` write THROUGH a
# nested `ContainerRef` cell instead of overwriting it (mirroring its
# existing `HashEntryRef` materialization branch).

# 1: the ticket's exact repro -- bind inside a named sub, write through the
# alias from mainline afterwards.
{
    my $var = 100;
    my $alias;
    sub bindit { $alias := $var }
    bindit();
    $alias = 5;
    is $var, 5, 'reverse-direction write: $alias = 5 reaches $var bound inside a sub';
}

# 2: control -- the already-working non-nested (top-level bind) case must
# keep working.
{
    my $x = 5;
    my \x := $x;
    x = 10;
    is $x, 10, 'top-level (non-nested) := bind still writes through on assignment';
}

{
    my $x2 = 5;
    my \x2 := $x2;
    $x2 = 20;
    is x2, 20, 'top-level (non-nested) := bind still reflects a source write';
}

# 3: two independent binds to the same source from two different subs --
# both aliases must observe each other's writes AND the source's writes,
# transitively through the shared cell.
{
    my $var = 100;
    my ($alias1, $alias2);
    sub bind1 { $alias1 := $var }
    sub bind2 { $alias2 := $var }
    bind1();
    bind2();

    $alias1 = 7;
    is $alias2, 7, 'two binds: alias2 observes a write through alias1';
    is $var, 7, 'two binds: source observes a write through alias1';

    $alias2 = 42;
    is $alias1, 42, 'two binds: alias1 observes a write through alias2';
    is $var, 42, 'two binds: source observes a write through alias2';

    $var = 99;
    is $alias1, 99, 'two binds: alias1 observes a direct source write';
    is $alias2, 99, 'two binds: alias2 observes a direct source write';
}

# 4: repeated writes through the same alias keep working (no one-shot fix).
{
    my $var = 1;
    my $alias;
    sub bindit4 { $alias := $var }
    bindit4();
    $alias = 2;
    $alias = 3;
    $alias = 4;
    is $var, 4, 'repeated writes through a sub-bound alias all reach the source';
}

# 5: writing through the alias, then reading the alias itself, still works
# (round-trip, not just source-side visibility).
{
    my $var = 1;
    my $alias;
    sub bindit5 { $alias := $var }
    bindit5();
    $alias = 55;
    is $alias, 55, 'reading the alias itself reflects a write through the alias';
}

# 6: rebinding the alias to a different source afterwards still replaces the
# link cleanly (rebind must not be treated as a write-through).
{
    my $var_a = 1;
    my $var_b = 2;
    my $alias;
    sub bindit6a { $alias := $var_a }
    sub bindit6b { $alias := $var_b }
    bindit6a();
    $alias = 10;
    is $var_a, 10, 'rebind test: first bind writes through before rebinding';
    bindit6b();
    $alias = 20;
    is $var_b, 20, 'rebind test: alias correctly rebound to the second source';
    is $var_a, 10, 'rebind test: old source is untouched after rebinding';
}

done-testing;
