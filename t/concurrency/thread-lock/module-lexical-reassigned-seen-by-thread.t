use Test;
use lib 't/lib';
use ThreadModuleLexical;

# #11697: a `my` variable of a `module { ... }` block, reassigned by one of the
# module's subs AFTER a thread has started, must be seen by code already
# running in that thread: rakudo's closures share the one container. The issue's
# own repro reads a Channel with `for $c.list`, whose delivery delay is #12727,
# so this pins the visibility with `.receive` instead.

plan 5;

{
    my $h = make-handler();
    my $c = Channel.new;
    my $r = Channel.new;
    my $p = start {
        loop {
            my $v = $c.receive;
            last if $v eq 'stop';
            $r.send($h($v));
        }
    };
    $c.send(1);
    is $r.receive, 'default 1', 'the thread sees the initial module lexical';
    set-err sub ($x) { "custom $x" };
    $c.send(2);
    is $r.receive, 'custom 2', 'and a reassignment made after the thread started';
    $c.send('stop');
    await $p;
}

{
    my $c = Channel.new;
    my $r = Channel.new;
    my $p = start {
        loop {
            my $v = $c.receive;
            last if $v eq 'stop';
            $r.send(peek());
        }
    };
    $c.send(1);
    is $r.receive, '0/none', 'module scalar and hash before any write';
    bump();
    bump();
    $c.send(2);
    is $r.receive, '2/2', 'main-thread writes are seen by the running thread';
    await start { bump() };
    $c.send(3);
    is $r.receive, '3/3', 'a write from a second thread is seen as well';
    $c.send('stop');
    await $p;
}
