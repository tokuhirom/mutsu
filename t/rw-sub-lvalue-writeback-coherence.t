use Test;

plan 8;

# Slice F (env<->locals coherence, docs/env-locals-coherence.md): an `is rw`
# subroutine that returns an lvalue (`sub () is rw { $v }`) lets the caller
# assign through the call (`f() = 9`), which writes the target variable in env
# by name. The call-site op now writes that value straight through to the
# caller's local slot, so the caller observes the update without the reverse
# `sync_locals_from_env` pull.

# --- direct rw sub returning a captured scalar -----------------------------
{
    my $v = 2;
    my $f = sub () is rw { $v };
    $f() = 9;
    is $v, 9, 'rw sub lvalue assignment updates the target variable';
    is $v + 1, 10, 'caller slot stays coherent for a following read';
}

# --- forwarding rw sub (assignment flows through a nested rw sub) ----------
{
    my $w = 1;
    my $leaf = sub () is rw { $w };
    my $fwd = sub () is rw { $leaf() };
    $fwd() = 42;
    is $w, 42, 'rw sub forwarding through a nested rw sub reaches the target';
}

# --- repeated assignments through the same rw sub --------------------------
{
    my $n = 0;
    my $acc = sub () is rw { $n };
    $acc() = 5;
    $acc() = 11;
    is $n, 11, 'a second rw-sub lvalue assignment overwrites the target';
}

# --- non-rw sub assignment dies, target unchanged --------------------------
{
    my $value = 7;
    my $ro = sub () { $value };
    dies-ok { $ro() = 3 }, 'assigning through a non-rw sub dies';
    is $value, 7, 'a failed non-rw assignment leaves the target unchanged';
}

# --- rw sub used as an lvalue then read back inside the same caller --------
{
    my $score = 100;
    my $ref = sub () is rw { $score };
    $ref() = 250;
    my $doubled = $score * 2;
    is $score, 250, 'rw-sub-assigned target is coherent for later reads';
    is $doubled, 500, 'the interleaved read saw the assigned value';
}
