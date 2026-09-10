use Test;

# env<->locals coherence (env_dirty substrate, docs/captured-outer-cell-sharing.md
# §10): a `proto method` dispatches its matching `multi method` candidate through
# the proto body's `{*}`, which runs via the slow `run_instance_method_resolved`
# path. That path merges the candidate's captured-outer caller-scalar writes back
# into env but does NOT set `env_dirty` or record a precise writeback, so the
# owning caller *local slot* was refreshed only by the call site's blanket pull
# (a no-op once env_dirty is gone). The proto call site now drains a precise
# `pending_caller_var_writeback` recorded by snapshotting env around the dispatch.
# Each subtest must hold with the blanket reconcile ON (default) AND OFF
# (`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1`). Read the captured
# var via `is`, which triggers a reconcile site in default builds.

plan 5;

# 1. Single proto+multi candidate mutating a captured outer scalar.
{
    my $r = '';
    class A1 {
        proto method l (|) { * }
        multi method l ($x) { $r ~= 'X'; $r }
    }
    A1.new.l(1);
    is $r, 'X', 'proto+multi candidate write reaches caller slot';
}

# 2. Candidate re-dispatching via &?ROUTINE.dispatcher() (the defer-next shape):
#    both the first and the re-dispatched candidate writes must accumulate.
{
    my $r = '';
    class A2 {
        proto method l (|) { * }
        multi method l ( &t, *@list ) { $r ~= '&'; $r ~= @list.join; $r }
        multi method l ( %t, *@list ) {
            $r ~= '%'; $r ~= @list.join;
            &?ROUTINE.dispatcher()( self, { %t{$^a} }, @list );
        }
        multi method l ( @t, *@list ) {
            $r ~= '@'; $r ~= @list.join;
            &?ROUTINE.dispatcher()( self, { @t[$^a] }, @list );
        }
    }
    my $a = A2.new;
    $a.l( {$_}, 1,2,3 );
    is $r, '&123', 'code-ref candidate write reaches caller slot';
    $r = '';
    $a.l( my %h, 4,5,6 );
    is $r, '%456&456', 'hash candidate + re-dispatch writes accumulate in caller slot';
    $r = '';
    $a.l( my @arr, 7,8,9 );
    is $r, '@789&789', 'array candidate + re-dispatch writes accumulate in caller slot';
}

# 3. Multiple distinct captured-outer scalars written by one candidate.
{
    my $p = 0;
    my $q = 0;
    class A3 {
        proto method m (|) { * }
        multi method m ($n) { $p = $n; $q = $n * 2 }
    }
    A3.new.m(7);
    is "$p $q", '7 14', 'multiple captured scalars all reach caller slots';
}
