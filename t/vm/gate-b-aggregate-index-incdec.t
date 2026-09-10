# (B)-gate: `%h`/`@a` aggregate index inc/dec must stay env-authoritative.
#
# The per-store env-write gate (MUTSU_GATE_LOCAL_ENV_WRITE) only skips the env
# mirror for `plain_locals` scalars, so a `%`/`@` aggregate keeps a fresh env
# while its local slot may hold a stale early snapshot. #4890 read/mutated the
# inc/dec container slot-first for every name, which lost a `%h is MixHash;
# %h<a>--` update under the gate (roast S02-types/mixhash.t, sethash.t). The
# `gate_local_slot` plain-locals guard restricts slot-first to scalars.
#
# Runs under both gate states (the harness may set MUTSU_GATE_LOCAL_ENV_WRITE);
# results must be identical either way.
use Test;

plan 10;

{
    my %h is MixHash = a => 1, c => 1;
    %h<c>++;
    is %h<c>, 2, 'MixHash %h<c>++ increments';
    %h<a>--;
    is %h.keys.sort.join(','), 'c', 'MixHash %h<a>-- to zero removes the key';
    %h<a>--;
    is %h<a>, -1, 'MixHash %h<a>-- goes negative';
    ok %h<a>:exists, 'MixHash negative key exists';
}

{
    my %h is SetHash = a => True, c => True;
    %h<c>--;
    nok %h<c>:exists, 'SetHash %h<c>-- removes the member';
    %h<b>++;
    ok %h<b>:exists, 'SetHash %h<b>++ adds the member';
}

{
    my @a = 10, 20, 30;
    @a[1]++;
    is @a[1], 21, 'plain @a[1]++ increments';
    @a[4]++;
    is @a[4], 1, 'plain @a autoviv element ++ from hole';
}

{
    my %h = x => 5;
    %h<x>--;
    is %h<x>, 4, 'plain %h<x>-- decrements';
    %h<y>++;
    is %h<y>, 1, 'plain %h<y>++ autovivs to 1';
}
