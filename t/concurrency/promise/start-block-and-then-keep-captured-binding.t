use v6;
use Test;

# A `.then` callback or `start` block that captures a routine / pointy-block
# PARAMETER must keep reading that parameter's own value. The cross-thread
# shared store is keyed by bare name, and it used to pull an unrelated
# same-named lexical over the capture at the worker's next sync point (`sleep`,
# `.result`, `await`): here the caller's loop re-declares `my $job;` (Any)
# before the callback runs. Regression pin for the JobQueue distribution,
# whose Queue then-hook saw its `$job` parameter turn into `Any` and died
# with "No such method 'finish' for invocant of type 'Any'".

plan 4;

# `.then` callback capturing a sub parameter (a Str: a plain scalar).
{
    my $g = Promise.new;
    my $done;
    my @pending = 'JOB';
    sub run-it(&c) { c() }
    sub start-it($job) {
        $done = $g.then: -> $p { sleep 0.05; $job };
    }
    loop {
        my $job;
        run-it { $job = @pending.shift if @pending };
        last without $job;
        start-it($job);
    }
    $g.keep(1);
    is await($done), 'JOB', '.then callback keeps its captured sub parameter';
}

# `.then` callback capturing a method parameter, driven from a queue-style
# `loop { my $job; $lock.protect: { ... }; ... }`.
{
    class Q {
        has @!pending;
        has Lock $!lock = Lock.new;
        has &.run-job;
        has $.done is rw;
        method enqueue($j) { $!lock.protect: { @!pending.push: $j }; self.tick; $j }
        method tick {
            loop {
                my $job;
                $!lock.protect: { $job = @!pending.shift if @!pending };
                last without $job;
                self!start($job);
            }
        }
        method !start($job) {
            my $promise = &!run-job($job);
            $!done = $promise.then: -> $p { my $r = $p.result; "$job/$r" };
        }
    }
    my $g = Promise.new;
    my $q = Q.new(run-job => -> $j { start { await $g; 'done' } });
    $q.enqueue('JOB');
    $g.keep(1);
    is await($q.done), 'JOB/done', '.then callback keeps its captured method parameter';
}

# `start` block capturing a pointy-block parameter that holds a Hash (not a
# plain scalar), where the first spawn of the program happens inside that call
# and later runners are started from `.then` callbacks.
{
    class SQ {
        has @!pending;
        has %!running;
        has Lock $!lock = Lock.new;
        has &.run-job;
        method enqueue($j) { $!lock.protect: { @!pending.push: $j }; self.tick; $j }
        method tick {
            loop {
                my $job;
                $!lock.protect: {
                    if @!pending && !%!running {
                        $job = @!pending.shift;
                        %!running{$job<id>} = $job;
                    }
                }
                last without $job;
                self!start($job);
            }
        }
        method !start($job) {
            my $promise = &!run-job($job);
            $promise.then: -> $p {
                $job<done>.keep(1);
                $!lock.protect: { %!running{$job<id>}:delete };
                self.tick;
            };
        }
    }
    my @order;
    my %gates;
    my $q = SQ.new(run-job => -> $job {
        @order.push: "start-$job<id>";
        my $g = Promise.new;
        %gates{$job<id>} = $g;
        start { await $g; @order.push: "end-$job<id>"; 'done' }
    });
    my @jobs;
    @jobs.push: $q.enqueue(%( id => $_, done => Promise.new )) for ^3;
    for @jobs -> $j {
        until %gates{$j<id>}:exists { await Promise.in(0.01) }
        %gates{$j<id>}.keep;
        await $j<done>;
    }
    is-deeply @order.List, <start-0 end-0 start-1 end-1 start-2 end-2>,
        'start block keeps its captured pointy-block parameter';
}

# A readonly parameter is still the one object: a worker's mutation through it
# is visible to the caller.
{
    sub f($c) { start { $c.send(42) }; $c.receive }
    is f(Channel.new), 42, 'a captured parameter still shares its object with the worker';
}
