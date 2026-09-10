use Test;

plan 16;

# Thread.is-initial-thread
ok Thread.is-initial-thread, 'Thread.is-initial-thread is True on main thread';
ok $*THREAD.is-initial-thread, '$*THREAD.is-initial-thread is True on main thread';

# Thread.start and isa-ok
{
    my $t = Thread.start({ 1 });
    isa-ok $t, Thread;
    $t.finish;
}

# Code in thread runs, is-initial-thread is False in spawned threads
{
    my $t = Thread.start({
        nok Thread.is-initial-thread, 'Thread.is-initial-thread is False in spawned thread';
        nok $*THREAD.is-initial-thread, '$*THREAD.is-initial-thread is False in spawned thread';
        pass "Code in thread ran";
    });
    $t.finish;
    pass "Thread was finished";
}

# Thread name
{
    my $t = Thread.start(:name("test thread"), { 1 });
    is $t.name, "test thread", "Thread has correct name";
    $t.finish;
}

{
    my $t = Thread.start({ 1 });
    is $t.name, "<anon>", "Default thread name is <anon>";
    $t.finish;
}

# Thread id
{
    my $t1 = Thread.start({ 1 });
    my $t2 = Thread.start({ 1 });
    isnt $t1.id, 0, "Thread has non-zero ID";
    isnt $t1.id, $t2.id, "Different threads have different IDs";
    $t1.finish;
    $t2.finish;
}

# Thread Str
{
    my $t = Thread.start({ 1 });
    ok $t.Str ~~ /^ Thread '<' \d+ '>' '(<anon>)' $/,
        "Thread stringification includes id and name";
    $t.finish;
}

# $*THREAD.id consistency
{
    my $tid;
    my $t = Thread.start({ $tid = $*THREAD.id; });
    $t.finish;
    is $tid, $t.id, '$*THREAD.id inside thread matches $t.id';
}

# $*THREAD on main thread
{
    isa-ok $*THREAD, Thread, '$*THREAD is a Thread on main thread';
    isnt $*THREAD.id, 0, 'Main thread has a non-zero ID';
}

# app_lifetime thread
{
    my $start = now;
    my $alt = Thread.start({ sleep 10000 }, :app_lifetime);
    ok now - $start < 5, "app_lifetime thread doesn't block main thread";
}
