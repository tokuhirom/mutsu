use Test;

# `Supply.interval(period, delay, :$scheduler)` hands its ticks to the given
# Scheduler instead of starting a timer, so a scheduler that controls its own
# clock drives the emissions deterministically. The tick block reaches the
# scheduler through the ordinary `.cue(&code, :every, :in)` call, which is what
# makes a *user-written* scheduler work -- roast's `Test::Tap::FakeScheduler` is
# exactly such a class.

plan 6;

my class TestScheduler does Scheduler {
    has $.time = 0;
    has @.upcoming;

    method cue(&code, :$every, :$in = 0, *%unknown) {
        die "TestScheduler does not understand: %unknown.keys().join(', ')" if %unknown;
        if $every {
            my $deadline = $!time + $in;
            for ^10 {
                @!upcoming.push($deadline => &code);
                $deadline += $every;
            }
        }
        else {
            code();
        }
    }

    method loads() { @!upcoming.elems }

    method progress-by($d) {
        $!time += $d;
        @!upcoming .= grep: {
            if .key <= $!time { .value.(); False } else { True }
        }
    }
}

{
    my $scheduler = TestScheduler.new;
    my $s = Supply.interval(1, :$scheduler);
    nok $s.live, 'a scheduler-driven interval is an on-demand Supply';
    my @res;
    $s.tap({ @res.push($_) });
    is-deeply @res, [], 'nothing is emitted until the scheduler advances';
    $scheduler.progress-by(4.5);
    is-deeply @res, [0, 1, 2, 3, 4], 'the interval counts up once per period';
    $scheduler.progress-by(2);
    is-deeply @res, [0, 1, 2, 3, 4, 5, 6], 'the counter continues across advances';
}

# The initial delay shifts the first tick, so a 4.5s advance of a 1s interval
# delayed by 2 yields three values rather than five.
{
    my $scheduler = TestScheduler.new;
    my @res;
    Supply.interval(1, 2, :$scheduler).tap({ @res.push($_) });
    $scheduler.progress-by(4.5);
    is-deeply @res, [0, 1, 2], 'the initial delay postpones the first tick';
}

# Nothing is cued before something taps: an untapped interval must not schedule.
{
    my $scheduler = TestScheduler.new;
    Supply.interval(1, :$scheduler);
    is $scheduler.loads, 0, 'an untapped interval cues nothing';
}

# vim: expandtab shiftwidth=4
