use v6;
use Test;

# Pin for the unregistered-timer-thread GC race: Promise.in/.at/.allof/.anyof
# (and other runtime service threads) used to run on raw, GC-unregistered
# threads, so a timer dropping its promise handle mid cycle-scan corrupted
# trial deletion (MUTSU_GC_VERIFY "survivor left inconsistent", strong N->N-1
# color Purple). Under the CI gc-stress job (MUTSU_GC=on +
# MUTSU_GC_EVERY_CANDIDATE + MUTSU_GC_VERIFY) this workload made collects race
# in-flight timers on nearly every run; the log gate fails on any VERIFY FAIL.

plan 3;

# Worker threads that build garbage cycles keep the candidate buffer busy so
# collects run concurrently with the timer threads below.
my @workers = (^4).map: {
    start {
        for ^200 {
            my %h;
            %h<self> := %h;
            my @a;
            @a[0] := @a;
        }
        True;
    }
};

my @timers;
for ^200 {
    @timers.push: Promise.in(0.001 + rand * 0.01);
    if @timers.elems > 40 {
        await @timers.shift;
    }
    my %h;
    %h<self> := %h;
}
await Promise.allof(@timers);
ok True, 'overlapping Promise.in timers all resolved during GC churn';

my $any = Promise.anyof(Promise.in(0.01), Promise.in(60));
await $any;
ok True, 'Promise.anyof resolved during GC churn';

await Promise.allof(@workers);
ok all(@workers.map(*.result)), 'cycle-churn workers all completed';
