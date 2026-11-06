use v6;
use Test;

# `Supply.throttle($n, &code)` limits how many jobs run at once; it does not
# serialize them. A callback that blocks (`sleep`, `await $proc`) must overlap
# with its siblings up to $n, and the results stay in input order. It used to
# run the callbacks one after the other on the calling thread. Found with
# Proc::Q (`:3batch` of three one-second processes took three seconds).
# The overlap is observed through a high-water mark, not a clock. Identical
# under raku.

plan 5;

sub run-throttled(Int $limit, Int $jobs) {
    my atomicint $running = 0;
    my atomicint $peak = 0;
    my $s = Supply.from-list(1..$jobs).throttle: $limit, -> $x {
        my $now = ++⚛$running;
        loop {
            my $seen = $peak;
            last if $now <= $seen;
            last if cas($peak, $seen, $now) == $seen;
        }
        sleep 0.3;
        --⚛$running;
        $x * 10
    };
    my @got;
    react whenever $s { @got.push(await $_) }
    (@got, $peak)
}

{
    my ($got, $peak) = run-throttled(3, 3);
    is $got.sort.join(","), "10,20,30", 'every job ran';
    is $peak, 3, 'three jobs were in flight at once';
}
{
    my ($got, $peak) = run-throttled(2, 4);
    is $got.sort.join(","), "10,20,30,40", 'four jobs, limit 2';
    is $peak, 2, 'never more than two in flight';
}
{
    my $s = Supply.from-list(1..3).throttle: 1, -> $x { $x + 1 };
    my @got;
    react whenever $s { @got.push(await $_) }
    is @got.join(","), "2,3,4", 'limit 1 keeps the input order';
}
