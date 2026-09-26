use v6;
use Test;

# ADR-0105 D2/D3 (#8380): an awaited promise bound to a user scheduler is
# woken through that scheduler's `.cue`, and the cued task does not complete
# until the resumed awaiter has reached its next blocking point. A
# virtual-time scheduler built the way `Test::Scheduler` 1.2 is (the nested
# delegation through `@*NESTED`, reduced to what these tests need) relies on
# exactly that: `advance-by` must not return before the woken code has
# registered its next timed event. Before ADR-0105 the second `sleep` below
# registered after the last `advance-by` and never woke (a deadlock).

plan 7;

class VirtualScheduler does Scheduler {
    has $!wrapped = $*SCHEDULER;
    has $.virtual-time = 0;
    has @!future;
    has $!lock = Lock.new;
    has @.cues;

    method cue(&code, :$in, :&catch, *%) {
        @!cues.push($in.defined ?? 'in' !! (&catch ?? 'catch' !! 'plain'));
        if $in {
            my $at = $!virtual-time + $in;
            $!lock.protect: { @!future.push: $at => &code };
        }
        else {
            with @*NESTED -> @nested {
                my $p = Promise.new;
                $!lock.protect: { @nested.push($p) };
                $!wrapped.cue({ code(); LEAVE $p.keep(True) });
            }
            else {
                $!lock.protect: { @!future.push: $!virtual-time => &code };
            }
        }
        Nil
    }

    method advance-by($seconds) {
        my $target = $!virtual-time + $seconds;
        loop {
            my @due = $!lock.protect: {
                my @now = @!future.grep(*.key <= $target).sort(*.key);
                @!future = @!future.grep(*.key > $target);
                @now
            };
            last unless @due;
            for @due -> $event {
                $!virtual-time = $event.key;
                my $done = Promise.new;
                $!wrapped.cue({
                    my @*NESTED = ();
                    $event.value.();
                    await @*NESTED;
                    LEAVE $done.keep(True);
                });
                await $done;
            }
        }
        $!virtual-time = $target;
    }

    method pending() { $!lock.protect: { @!future.elems } }
    method uncaught_handler() { $!wrapped.uncaught_handler }
    method loads() { $!wrapped.loads }
}

# The #8380 reduction, with `sleep` spelled as Test::Time spells it.
{
    my $s = VirtualScheduler.new;
    my sub vsleep($t) { await Promise.in($t, :scheduler($s)) }
    my $init = Promise.new;
    my @log;
    my $p = start {
        $init.keep;
        vsleep 10;
        @log.push: "first at {$s.virtual-time}";
        vsleep 20;
        @log.push: "second at {$s.virtual-time}";
    }
    await $init;
    # Not what this test is about (that the keeper reaches its first `vsleep`
    # before the main thread advances is ADR-0105 D2's ordering, pinned by the
    # scheduler oracle test): wait for the registration so the D3 assertions
    # below are the only ordering under test.
    sleep 0.01 until $s.pending;
    $s.advance-by: 10;
    is @log.join(','), 'first at 10', 'advance-by 10 wakes the first sleep before it returns';
    is $s.pending, 1, 'the resumed block registered its next sleep before advance-by returned';
    $s.advance-by: 20;
    is @log.join(','), 'first at 10,second at 30', 'advance-by 20 wakes the second sleep';
    await $p;
    ok $s.cues.grep(* ne 'in'), 'the awaiter wake-up was cued through the scheduler';
}

# The same shape with the awaiter on the main thread and the advancer on a
# pool worker (Test::Time's `:auto-advance`).
{
    my $s = VirtualScheduler.new;
    my sub vsleep($t) { await Promise.in($t, :scheduler($s)) }
    my $go = True;
    my $bg = Promise.start: { while $go { $s.advance-by: 1; } };
    vsleep 5;
    ok $s.virtual-time >= 5, 'main-thread awaiter woken by a worker-driven advance';
    vsleep 5;
    ok $s.virtual-time >= 10, 'and woken again for its next sleep';
    $go = False;
    await $bg;
}

# An already-resolved promise needs no wake-up and no cue.
{
    my $s = VirtualScheduler.new;
    my $p = Promise.new(scheduler => $s);
    $p.keep(42);
    my $before = $s.cues.elems;
    await $p;
    is $s.cues.elems, $before, 'awaiting a kept promise does not cue a wake-up';
}

# vim: expandtab shiftwidth=4
