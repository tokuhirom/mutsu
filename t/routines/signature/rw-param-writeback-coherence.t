use Test;

plan 16;

# Slice F (env<->locals coherence, docs/env-locals-coherence.md): an `is rw` /
# `is raw` parameter writes its final value back to the caller's *variable*.
# Historically the writeback went into `env` by name and relied on the reverse
# `sync_locals_from_env` pull to refresh the caller's compiled local slot before
# the next read. The slot is now written through directly at the call site, so
# the caller observes the new value coherently. These cases pin both the value
# and the coherence of subsequent slot reads.

# --- plain scalar rw on a named sub ---------------------------------------
{
    sub assign-rw($a is rw) { $a = 42 }
    my $x = 1;
    assign-rw($x);
    is $x, 42, 'rw scalar param writes back to caller variable';
    is $x + 1, 43, 'caller slot stays coherent for a following read';
}

# --- compound assignment through rw ---------------------------------------
{
    sub bump5($n is rw) { $n += 5 }
    my $y = 10;
    bump5($y);
    is $y, 15, 'compound assignment through rw param writes back';
    bump5($y);
    is $y, 20, 'a second rw call sees the updated slot value';
}

# --- rw param value AND return value ---------------------------------------
{
    sub bump-ret($n is rw) { $n += 5; $n * 2 }
    my $y = 10;
    my $r = bump-ret($y);
    is $r, 30, 'rw sub return value is computed from the mutated param';
    is $y, 15, 'rw sub also writes the param back to the caller';
}

# --- nested rw forwarding (write-through across two frames) ----------------
{
    sub inner($a is rw) { $a = 99 }
    sub outer($b is rw) { inner($b) }
    my $z = 1;
    outer($z);
    is $z, 99, 'rw forwarded through a nested call reaches the outer caller';
}

# --- is raw -----------------------------------------------------------------
{
    sub raw-assign($a is raw) { $a = 8 }
    my $v = 2;
    raw-assign($v);
    is $v, 8, 'is raw param writes back to caller variable';
}

# --- pointy-block rw param --------------------------------------------------
{
    my $bump = -> $v is rw { $v++ };
    my $n = 10;
    $bump($n);
    is $n, 11, 'rw pointy-block param writes back';
    $bump($n);
    is $n, 12, 'a second pointy-block call sees the updated slot';
}

# --- rw rejects a non-variable (literal) argument --------------------------
{
    sub need-rw($a is rw) { $a = 1 }
    dies-ok { need-rw(7) }, 'rw param rejects a literal argument';
}

# --- rw inside a loop (repeated write-through) -----------------------------
{
    sub inc($a is rw) { $a++ }
    my $count = 0;
    inc($count) for ^5;
    is $count, 5, 'rw param incremented across a loop writes through each time';
}

# --- two rw params ----------------------------------------------------------
{
    sub swap($a is rw, $b is rw) { my $t = $a; $a = $b; $b = $t }
    my ($p, $q) = 1, 2;
    swap($p, $q);
    is $p, 2, 'first rw param of two writes back';
    is $q, 1, 'second rw param of two writes back';
}

# --- rw param interleaved with a plain (readonly) param --------------------
{
    sub add-into($acc is rw, $delta) { $acc += $delta }
    my $sum = 100;
    add-into($sum, 23);
    is $sum, 123, 'rw param writes back while a readonly param does not';
    is $sum * 2, 246, 'caller slot remains coherent after a mixed-param call';
}
