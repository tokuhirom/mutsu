use Test;

# Pin for the (B) per-store env-write gate `let`/`temp` cluster fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown").
#
# `exec_let_save_op` snapshots the pre-scope value of the temporized variable.
# It used to read that value from `env` by name, falling back to the slot. Under
# the MUTSU_GATE_LOCAL_ENV_WRITE gate a plain-lexical assignment `$x = 1` before
# the `temp`/`let` skips its env mirror, so the env-first read would snapshot the
# `my $x` decl seed (Any) instead of 1 — and the scope-exit restore would then
# put Any back. The fix reads the baked slot first (always current). This passes
# gate-OFF (the default) and would fail gate-ON before the fix.

plan 6;

{
    my $x = 1;
    { temp $x = 42; is $x, 42, 'temp: value set inside block' }
    is $x, 1, 'temp: pre-scope slot value restored (not the stale env seed)';
}

{
    my $x = 10;
    { temp $x; $x = 99; is $x, 99, 'bare temp: value changed inside block' }
    is $x, 10, 'bare temp: pre-scope slot value restored';
}

{
    my $x = 7;
    try { let $x = 42; die 'oops' }
    is $x, 7, 'let: pre-scope slot value restored on exception';
}

{
    my $x = 3;
    { temp $x = 5; { temp $x = 9 }; is $x, 5, 'nested temp: inner restored to outer temp value' }
}
