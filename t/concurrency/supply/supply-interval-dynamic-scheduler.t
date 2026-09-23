use Test;

# Test::Scheduler 1.2 relies on Supply.interval using the dynamic scheduler
# when no explicit :scheduler argument is supplied.
class QueueScheduler does Scheduler {
    has @.callbacks;

    method cue(&code, :$every, :$in = 0, *%_) {
        @!callbacks.push(&code);
    }

    method uncaught_handler() { Nil }
    method loads() { @!callbacks.elems }
}

plan 3;

my $scheduler = QueueScheduler.new;
my @values;
{
    my $*SCHEDULER = $scheduler;
    Supply.interval(1).tap({ @values.push($_) });
}

is $scheduler.callbacks.elems, 1, 'Supply.interval cues the dynamic scheduler';
$scheduler.callbacks[0]();
is-deeply @values, [0], 'the dynamic scheduler drives the first interval tick';
is $scheduler.callbacks.elems, 1, 'the interval is represented by one scheduler cue';
