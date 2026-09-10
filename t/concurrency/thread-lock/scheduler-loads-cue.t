use Test;

plan 8;

# $*SCHEDULER.loads — outstanding scheduled-task count (0 when idle).
is $*SCHEDULER.loads, 0, '.loads is 0 on an idle scheduler';

# cue arg validation: mutually-exclusive scheduling adverbs throw synchronously.
dies-ok { $*SCHEDULER.cue({ 1 }, :at(now + 2), :in(1)) },
    'cue dies on :in + :at';
dies-ok { $*SCHEDULER.cue({ 1 }, :every(0.1), :times(10), :stop({ 1 })) },
    'cue dies on :every + :times + :stop';

# CurrentThreadScheduler runs cue synchronously.
{
    my $sched = CurrentThreadScheduler.new;
    dies-ok { $sched.cue({ 1 }, :every(1)) },
        'CurrentThreadScheduler cue dies on :every';

    my $ran = False;
    $sched.cue({ $ran = True });
    is $ran, True, 'sync cue runs the callback inline';
    is $sched.loads, 0, '.loads is 0 after a synchronous cue';

    # An uncaught exception from a sync cue (no :catch) goes to uncaught_handler.
    my $msg;
    $sched.uncaught_handler = sub ($ex) { $msg = $ex.message };
    $sched.cue({ die 'boom' });
    is $msg, 'boom', 'uncaught_handler receives a sync cue exception';
    $sched.uncaught_handler = Nil;

    # A :catch handler takes precedence and receives the exception.
    my $caught;
    $sched.cue({ die 'kaboom' }, :catch(-> $ex { $caught = $ex.message }));
    is $caught, 'kaboom', ':catch handler receives the exception';
}

done-testing;
