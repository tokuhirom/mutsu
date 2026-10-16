use Test;

# `Supplier`'s and the schedulers' methods are rows, found by their owner; a
# row runs one implementation whatever the receiver is (ADR-11276 §9.133).

plan 8;

# A supplier held in an attribute emits through the same `emit` as one held in
# a variable.
class Holder {
    has $.supplier = Supplier.new;
    method send($v) { $!supplier.emit($v) }
    method finish() { $!supplier.done }
}
{
    my $h = Holder.new;
    my @seen;
    my $done = False;
    $h.supplier.Supply.tap({ @seen.push($_) }, done => { $done = True });
    $h.send(1);
    $h.send(2);
    my $s = $h.supplier;
    $s.emit(3);
    $h.finish;
    is-deeply @seen, [1, 2, 3], 'emit through an attribute and a variable';
    ok $done, 'done through an attribute';
}

{
    my $p = Supplier::Preserving.new;
    $p.emit($_) for 1..3;
    $p.done;
    is-deeply $p.Supply.list, (1, 2, 3), 'Supplier::Preserving reaches the rows';
}

ok $*SCHEDULER.loads >= 0, 'ThreadPoolScheduler.loads';
ok $*SCHEDULER.max_threads > 0, 'ThreadPoolScheduler.max_threads';
{
    my $sched = CurrentThreadScheduler.new;
    my $ran = False;
    $sched.cue({ $ran = True });
    ok $ran, 'CurrentThreadScheduler.cue runs the block';
}
{
    my $old = $*SCHEDULER.uncaught_handler;
    my $handler = -> $ { };
    $*SCHEDULER.uncaught_handler = $handler;
    ok $*SCHEDULER.uncaught_handler === $handler, 'uncaught_handler round-trips';
    $*SCHEDULER.uncaught_handler = $old;
}
ok Supplier.new.can('emit'), '.can finds a row';
