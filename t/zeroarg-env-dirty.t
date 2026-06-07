use v6;
use Test;

# Regression pin for Slice 6.3 step 2: the 0-arg compiled-function fast path no
# longer sets a blanket `env_dirty = true` after every call. A pure 0-local
# function (no env writes) must NOT force a caller locals re-sync, but a function
# that writes a captured-outer variable (directly, via increment, via a nested
# call, or via an array/hash mutation) must still propagate to the caller.
# See docs/vm-dual-store.md (Slice 6.3, has_env_writes gate).

plan 16;

# --- pure constant function: caller locals stay coherent across a loop ---
{
    sub pure-const { 42 }
    my $sum = 0;
    my $iters = 0;
    for ^50 {
        $sum = $sum + pure-const();
        $iters = $iters + 1;
    }
    is $sum, 2100, 'pure 0-arg fn result accumulates correctly';
    is $iters, 50, 'caller local untouched by pure 0-arg calls';
}

# --- function with only its own local is pure wrt the caller ---
{
    sub with-local { my $x = 10; $x * 2 }
    my $c = 0;
    for ^10 { $c = $c + with-local() }
    is $c, 200, 'local-only 0-arg fn does not disturb caller local';
}

# --- captured-outer scalar write via direct assignment ---
{
    my $acc = 0;
    sub bump { $acc = $acc + 5 }
    bump(); bump(); bump();
    is $acc, 15, 'captured-outer scalar assignment propagates';
}

# --- captured-outer write via increment ---
{
    my $n = 0;
    sub incn { $n++ }
    incn() for ^7;
    is $n, 7, 'captured-outer increment propagates';
}

# --- nested 0-arg call mutating an outer var ---
{
    my $log = '';
    sub note-x { $log = $log ~ 'x' }
    sub note-twice { note-x(); note-x() }
    note-twice();
    note-twice();
    is $log, 'xxxx', 'nested 0-arg calls propagate outer writes';
}

# --- captured-outer array push ---
{
    my @items;
    sub add-item { @items.push(99) }
    add-item() for ^4;
    is @items.elems, 4, 'captured-outer array push propagates';
    is @items[0], 99, 'pushed value correct';
}

# --- captured-outer hash store ---
{
    my %seen;
    my $i = 0;
    sub mark { %seen{"k$i"} = $i }
    for ^3 { mark(); $i = $i + 1 }
    is %seen.elems, 3, 'captured-outer hash store propagates';
}

# --- recursion through a 0-arg fn touching an outer counter ---
{
    my $depth = 0;
    sub rec { $depth = $depth + 1; if $depth < 5 { rec() } }
    rec();
    is $depth, 5, '0-arg recursion via outer counter terminates correctly';
}

# --- interleave pure calls with caller-local mutation ---
{
    sub gimme { 3 }
    my $total = 0;
    my $count = 0;
    for ^20 {
        $total = $total + gimme();
        $count = $count + 1;
    }
    is $total, 60, 'pure fn return interleaved with caller mutation';
    is $count, 20, 'caller loop counter coherent';
}

# --- a pure fn whose value feeds a condition, with an outer accumulator ---
{
    sub five { 5 }
    my $hits = 0;
    for 1..10 -> $x {
        if $x < five() { $hits = $hits + 1 }
    }
    is $hits, 4, 'pure fn in condition; outer accumulator coherent';
}

# --- empty-signature explicit fn ---
{
    sub answer() { 7 }
    my $s = 0;
    for ^6 { $s = $s + answer() }
    is $s, 42, 'empty-sig 0-arg fn pure across loop';
}

# --- a fn that both reads and writes an outer (read-modify-write) ---
{
    my $state = 1;
    sub doubler { $state = $state * 2 }
    doubler() for ^5;
    is $state, 32, 'read-modify-write of outer var via 0-arg fn';
}

# --- two outer vars, only one written ---
{
    my $written = 0;
    my $readonly = 100;
    sub touch-one { $written = $written + $readonly }
    touch-one(); touch-one();
    is $written, 200, 'fn writing one outer while reading another stays correct';
}
