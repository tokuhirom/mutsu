use Test;

plan 13;

# Slice F (env<->locals coherence, docs/env-locals-coherence.md), category 3:
# an `is rw` / `is raw` *method* parameter writes its final value back to the
# caller's variable. The writeback (`call_compiled_method` -> merge_method_env)
# updates the caller variable in `env` by name; the CallMethod / CallMethodMut op
# now writes that value straight through to the caller's local slot, so the slot
# stays coherent without the reverse `sync_locals_from_env` pull.

class C {
    method assign-rw($a is rw) { $a = 42 }
    method bump5($n is rw) { $n += 5 }
    method bump-ret($n is rw) { $n += 5; $n * 2 }
    method swap($a is rw, $b is rw) { my $t = $a; $a = $b; $b = $t }
    method inc($a is rw) { $a++ }
    method raw-assign($a is raw) { $a = 8 }
}

# --- scalar rw on a type-object method call --------------------------------
{
    my $x = 1;
    C.assign-rw($x);
    is $x, 42, 'rw method param writes back to caller variable';
    is $x + 1, 43, 'caller slot stays coherent for a following read';
}

# --- compound assignment through rw ----------------------------------------
{
    my $y = 10;
    C.bump5($y);
    is $y, 15, 'compound assignment through rw method param writes back';
    C.bump5($y);
    is $y, 20, 'a second rw method call sees the updated slot';
}

# --- rw method value AND return value --------------------------------------
{
    my $y = 10;
    my $r = C.bump-ret($y);
    is $r, 30, 'rw method return value computed from the mutated param';
    is $y, 15, 'rw method also writes the param back to the caller';
}

# --- two rw params ----------------------------------------------------------
{
    my ($p, $q) = 1, 2;
    C.swap($p, $q);
    is $p, 2, 'first rw method param of two writes back';
    is $q, 1, 'second rw method param of two writes back';
}

# --- is raw method param ----------------------------------------------------
{
    my $v = 2;
    C.raw-assign($v);
    is $v, 8, 'is raw method param writes back to caller variable';
}

# --- rw method call on an instance -----------------------------------------
{
    my $c = C.new;
    my $n = 100;
    $c.inc($n);
    is $n, 101, 'rw method param on an instance receiver writes back';
    $c.inc($n);
    is $n, 102, 'a second instance-method rw call sees the updated slot';
}

# --- rw method call in a loop ----------------------------------------------
{
    my $count = 0;
    C.inc($count) for ^5;
    is $count, 5, 'rw method param incremented across a loop writes through each time';
}

# --- rw method rejects a literal argument ----------------------------------
{
    dies-ok { C.assign-rw(7) }, 'rw method param rejects a literal argument';
}
