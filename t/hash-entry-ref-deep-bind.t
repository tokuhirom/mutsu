# The deferred missing-key bind token (the last `SlotRef`-era survivor) is now a
# single path-based `HashEntryRef` carrying the full key path, instead of a
# `HashSlotRef` + a one-level-deep `DeferredHashAccess` chain. Besides the
# behavior preserved by `t/phantom-entry-bind.t`, the path representation
# naturally supports binds deeper than two missing levels, which the old
# two-variant chain did not.
use Test;

plan 9;

# --- 3 fully-missing levels: lazy until the first write, then materialized ---
{
    my %h;
    my $b := %h<a><b><c>;
    nok %h<a>:exists, '3-level: nothing autovivified before write';
    nok $b.defined, '3-level: reads as undefined before write';
    $b = 5;
    is %h<a><b><c>, 5, '3-level: write materializes the whole path';
    ok %h<a><b>:exists, '3-level: intermediate exists after write';
    # bidirectional alias after materialization
    %h<a><b><c> = 6;
    is $b, 6, '3-level: bound var sees a later write through the hash';
    $b = 7;
    is %h<a><b><c>, 7, '3-level: hash sees a later write through the bound var';
}

# --- 4 missing levels ---
{
    my %h;
    my $d := %h<w><x><y><z>;
    $d = 42;
    is %h<w><x><y><z>, 42, '4-level: deep write materializes the path';
}

# --- a deferred deep bind is NOT retro-bound by an independent hash write ---
{
    my %g;
    my $c := %g<p><q><r>;
    %g<p><q><r> = 99;
    nok $c.defined, 'deep: independent hash write does not retro-bind the var';
}

# --- read before write does not autovivify any level ---
{
    my %m;
    my $e := %m<one><two><three>;
    my $ignore = $e;
    nok %m<one>:exists, 'deep: read of the bound var does not autovivify';
}
