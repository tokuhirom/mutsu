use Test;

plan 4;

{
    my $tracker = 0;
    my $c = $*SCHEDULER.cue({ cas $tracker, {.succ} }, :times(10));
    isa-ok($c, Cancellation);
    is $tracker, 10, 'default scheduler respects :times';
    LEAVE .cancel with $c;
}

{
    $*SCHEDULER = CurrentThreadScheduler.new;
    my $tracker;
    my $c = $*SCHEDULER.cue({ $tracker++ }, :times(10));
    ok $c.can("cancel"), 'cue returns a cancelable handle';
    is $tracker, 10, 'current thread scheduler respects :times';
    LEAVE .cancel with $c;
}
