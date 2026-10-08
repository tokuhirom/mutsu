use Test;

# Found by Test::Time (ecosystem distribution): Promise.in hands its delay to a
# custom scheduler's `cue(:in)` unchanged. Coercing an Int to a Num made
# `Instant + $in` on a virtual clock pick up f64 noise (9.999999903 vs 10).

plan 5;

class VirtualScheduler does Scheduler {
    has Instant $.virtual-time = now;
    has @.seen;
    has @.queue;
    method cue(&code, :$in, :$at, :$every, :$times, :&stop, :&catch) {
        @!seen.push($in);
        @!queue.push: ($!virtual-time + ($in // 0), &code);
        Nil
    }
    method uncaught_handler is rw { $ }
    method loads { 0 }
}

my $s = VirtualScheduler.new;
my $p = Promise.in(10, :scheduler($s));
isa-ok $s.seen[0], Int, 'an Int delay reaches cue(:in) as an Int';
is-deeply $s.seen[0], 10, 'and keeps its value';

my ($when, &code) = $s.queue[0].list;
is $when - $s.virtual-time, 10, 'Instant + delay is exact';

my $q = Promise.in(0.5, :scheduler($s));
isa-ok $s.seen[1], Rat, 'a Rat delay reaches cue(:in) as a Rat';

&code();
await $p;
is $p.status, Kept, 'the cued block keeps the promise';
