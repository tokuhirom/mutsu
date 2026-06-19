use v6;
use Test;

# Slice F (env<->locals coherence) pin: a 0-arg compiled-function fast-call
# (`call_compiled_function_fast`) that mutates a *captured-outer* variable by
# name must write the new value straight through to the caller's local slot at
# the call site, so the caller stays coherent WITHOUT the reverse
# `sync_locals_from_env` pull. The fix records the body's `free_var_writes` and
# drains them via `apply_pending_rw_writeback`, the 0-arg analog of the closure
# captured-var writeback (#3307).
#
# Scope: this is the *single-level* (directly-enclosing caller) case. A write a
# deeper callee performs against a grand-ancestor's variable (nested 0-arg calls,
# 0-arg recursion) still depends on the reverse pull and is deferred — carrying
# writeback sources up the call stack collides with lazy-iteration topics.
#
# This pin must pass identically with the reverse pull enabled (default) and
# disabled (`MUTSU_NO_REVERSE_SYNC=1`).

plan 10;

# --- direct captured-outer scalar assignment ---
{
    my $acc = 0;
    sub fc-bump { $acc = $acc + 5 }
    fc-bump(); fc-bump(); fc-bump();
    is $acc, 15, 'captured-outer scalar assignment propagates through fast call';
}

# --- captured-outer increment ---
{
    my $n = 0;
    sub fc-incn { $n++ }
    fc-incn() for ^7;
    is $n, 7, 'captured-outer increment propagates';
}

# --- read-modify-write ---
{
    my $state = 1;
    sub fc-doubler { $state = $state * 2 }
    fc-doubler() for ^5;
    is $state, 32, 'captured-outer read-modify-write propagates';
}

# --- writes one outer, reads another ---
{
    my $written = 0;
    my $readonly = 100;
    sub fc-touch { $written = $written + $readonly }
    fc-touch(); fc-touch();
    is $written, 200, 'writes one captured var while reading another';
    is $readonly, 100, 'read-only captured var unchanged';
}

# --- captured-outer array push (container path stays coherent too) ---
{
    my @items;
    sub fc-add { @items.push(99) }
    fc-add() for ^4;
    is @items.elems, 4, 'captured-outer array push propagates';
    is @items[0], 99, 'pushed value correct';
}

# --- pure fn interleaved with caller-local mutation stays independent ---
{
    sub fc-pure { 42 }
    my $sum = 0;
    my $iters = 0;
    for ^10 {
        $sum = $sum + fc-pure();
        $iters = $iters + 1;
    }
    is $sum, 420, 'pure fast call result accumulates; caller local coherent';
    is $iters, 10, 'caller loop counter untouched by pure fast calls';
}

# --- two independent captured vars written by two different subs ---
{
    my $a = 0;
    my $b = 0;
    sub fc-wa { $a = $a + 1 }
    sub fc-wb { $b = $b + 10 }
    fc-wa(); fc-wb(); fc-wa(); fc-wb();
    is "$a/$b", "2/20", 'two distinct captured writers stay independent';
}
