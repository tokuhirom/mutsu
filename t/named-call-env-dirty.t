use v6;
use Test;

# Regression pin for Slice 6.3 step 2: the heavy "named" compiled-function
# dispatch path (call_compiled_function_named, used for complex signatures —
# default params, return types, where-constraints, typed params) no longer sets
# a blanket env_dirty after every call. It signals env_dirty *precisely* from
# its return merge (a captured-outer / is-rw writeback, or an is-raw return).
# Caller locals must stay coherent across such calls, and writebacks must
# propagate. See docs/vm-dual-store.md (Slice 6.3).

plan 16;

# --- default-param fn: caller locals coherent across a loop ---
{
    sub g($x = 5) { $x + 1 }
    my $c = 0;
    my $iters = 0;
    for ^40 { $c = $c + g(2); $iters = $iters + 1 }
    is $c, 120, 'default-param fn result accumulates correctly';
    is $iters, 40, 'caller local untouched by default-param calls';
}

# --- return-type fn ---
{
    sub h(--> Int) { 7 }
    my $c = 0;
    for ^10 { $c = $c + h() }
    is $c, 70, 'return-type fn pure across loop';
}

# --- where-constraint param ---
{
    sub w($x where * > 0) { $x * 2 }
    my $c = 0;
    for ^10 { $c = $c + w(3) }
    is $c, 60, 'where-constraint fn pure across loop';
}

# --- typed param (fast type) fn looping ---
{
    sub typed(Int $x) { $x + 100 }
    my $acc = 0;
    for ^5 { $acc = $acc + typed(1) }
    is $acc, 505, 'typed-param fn caller-local coherence';
}

# --- captured-outer write via a default-param fn ---
{
    my $o = 0;
    sub addit($x = 1) { $o = $o + $x }
    addit(5) for ^3;
    is $o, 15, 'captured-outer write via heavy path propagates';
}

# --- is rw param writeback ---
{
    sub inc($x is rw) { $x++ }
    my $n = 10;
    inc($n); inc($n); inc($n);
    is $n, 13, 'is rw param writeback propagates to caller';
}

# --- is rw in a loop, caller has other locals ---
{
    sub bump2($x is rw) { $x = $x + 2 }
    my $v = 0;
    my $other = 0;
    for ^5 { bump2($v); $other = $other + 1 }
    is $v, 10, 'is rw writeback coherent across loop';
    is $other, 5, 'unrelated caller local coherent alongside is rw call';
}

# --- dynamic var write through heavy path ---
{
    my $*D = 0;
    sub bumpd($x = 1) { $*D = $*D + $x }
    bumpd(2) for ^3;
    is $*D, 6, 'dynamic var write through heavy path propagates';
}

# --- nested heavy calls; outer accumulator coherent ---
{
    sub leaf($x = 1) { $x * 10 }
    sub mid($y = 2) { leaf($y) + 1 }
    my $sum = 0;
    for ^6 { $sum = $sum + mid(3) }
    is $sum, 186, 'nested heavy-path calls keep caller accumulator coherent';
}

# --- heavy fn returning a value used as a condition, with outer state ---
{
    sub threshold($base = 0) { $base + 5 }
    my $hits = 0;
    for 1..10 -> $x {
        if $x < threshold() { $hits = $hits + 1 }
    }
    is $hits, 4, 'heavy fn in condition; outer accumulator coherent';
}

# --- read-modify-write of an outer var via a default-param fn ---
{
    my $state = 1;
    sub doubler($f = 2) { $state = $state * $f }
    doubler() for ^5;
    is $state, 32, 'read-modify-write of outer via heavy-path fn';
}

# --- two outer vars, only one written, through heavy path ---
{
    my $written = 0;
    my $ro = 100;
    sub touch($k = 1) { $written = $written + $ro * $k }
    touch(); touch();
    is $written, 200, 'heavy fn writing one outer while reading another';
}

# --- interleave pure heavy calls with caller mutation ---
{
    sub five-ish($b = 5) { $b }
    my $total = 0;
    my $count = 0;
    for ^20 { $total = $total + five-ish(); $count = $count + 1 }
    is $total, 100, 'pure heavy fn return interleaved with caller mutation';
    is $count, 20, 'caller loop counter coherent';
}
