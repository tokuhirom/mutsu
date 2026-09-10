use Test;

# env<->locals coherence (env_dirty substrate, docs/captured-outer-cell-sharing.md
# §10): an `EVAL` carrier that reassigns a captured-outer caller variable to a
# plain scalar (`EVAL q'$x = 1'`) writes the new value into env, and the carrier
# writeback reconciles it into the caller's local slot. The reconcile eligibility
# is on the *new env value* (a plain scalar carries no `:=` cell), so it must fire
# even when the slot previously held a container (`my $x = []; EVAL q'$x = 1'`) —
# the old "slot is a scalar" test missed that, leaving it to the blanket reconcile
# (a no-op under double-OFF). Each subtest must hold with blanket reconcile ON
# (default) AND OFF (`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1`).

plan 6;

{
    my $a = [];
    EVAL q'$a = 1';
    is $a, 1, 'EVAL scalar assignment overwrites a container slot';
}
{
    my $b = [];
    EVAL q'$b = do { 5 }';
    is $b, 5, 'EVAL do-block scalar assignment overwrites a container slot';
}
{
    my $c = {};
    EVAL q'$c = 7 + 2';
    is $c, 9, 'EVAL scalar assignment overwrites a hash-valued slot';
}
{
    # The auto-curly terminator case from roast/S04-statements/terminator.t.
    my $z = [];
    EVAL q'
        $z = do { 1 }
                + 2;
    ';
    is $z, 1, 'auto-curly applies inside array composer (EVAL writeback)';
}
{
    # A scalar slot reassigned to a scalar must still work (regression guard).
    my $d = 0;
    EVAL q'$d = 42';
    is $d, 42, 'EVAL scalar->scalar assignment still reconciles';
}
{
    # A container slot reassigned to a *container* must NOT be clobbered
    # incorrectly: the value still propagates and round-trips.
    my @e = 1, 2, 3;
    EVAL q'@e = 7, 8';
    is @e.join(','), '7,8', 'EVAL container reassignment still reconciles';
}
