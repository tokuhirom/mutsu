use v6;
use Test;

# Low-level `Thread` construction/starting and the two recursion-aware
# `Lock::Async` methods. Every expectation here was established by running the
# same code under `raku` v2026.06 first; see
# `news/2026-08/thread-run-and-lock-async-recursion.md`.
#
# Determinism rules for this file: every thread that is started is explicitly
# `.finish`/`.join`ed, every queued Promise is `await`ed before its effect is
# observed, and nothing asserts on interleaving order that the language does
# not guarantee.

plan 31;

# --- Thread.new: constructed but NOT started -----------------------------

{
    my $ran = Channel.new;
    my $t = Thread.new(code => { $ran.send('ran') });
    isa-ok $t, Thread, 'Thread.new returns a Thread';
    is $t.name, '<anon>', 'Thread.new default name is <anon>';
    nok $t.app_lifetime, 'Thread.new default app_lifetime is False';
    ok $t.id ~~ Int, 'a not-yet-started Thread already has an Int id';
    ok $t.id > 0, 'that id is non-zero';
    is $t.Numeric, $t.id, 'Thread.Numeric is the thread id';
    ok $t.Str ~~ /^ 'Thread<' \d+ '>(<anon>)' $/, 'Thread.Str is Thread<id>(name)';

    my $returned = $t.run;
    ok $returned === $t, 'Thread.run returns the invocant';
    $t.finish;
    $ran.close;
    is $ran.list.join(','), 'ran', 'the code passed to Thread.new ran on .run';
}

{
    my $t = Thread.new(code => { 1 }, name => 'named thread', :app_lifetime);
    is $t.name, 'named thread', 'Thread.new honours :name';
    ok $t.app_lifetime, 'Thread.new honours :app_lifetime';
    ok $t.Str ~~ /^ 'Thread<' \d+ '>(named thread)' $/,
        'Thread.Str shows the given name';
    # An :app_lifetime thread is never joined by the runtime, so run and join
    # it here rather than leaving it to race with process exit.
    $t.run.finish;
}

{
    my $t1 = Thread.new(code => { 1 });
    my $t2 = Thread.new(code => { 1 });
    isnt $t1.id, $t2.id, 'each Thread.new gets its own id';
    $t1.run.finish;
    $t2.run.finish;
}

# --- Thread.start: constructed and started in one step -------------------

{
    my $seen = Channel.new;
    my $t = Thread.start(:name('worker'), { $seen.send($*THREAD.id) });
    is $t.name, 'worker', 'Thread.start honours :name';
    $t.join;
    $seen.close;
    is $seen.list.join(','), $t.id.Str, '$*THREAD.id inside matches the Thread id';
}

# --- Lock::Async.protect-or-queue-on-recursion ---------------------------

{
    my Lock::Async $lock .= new;
    my Int $count = 0;
    my $r = $lock.protect-or-queue-on-recursion({ $count++ });
    nok $r.defined, 'a non-recursive protect-or-queue-on-recursion returns an undefined value';
    is $count, 1, 'and it ran the block immediately, like .protect';

    # Locked, but not by anything on this caller chain -- still behaves like
    # .protect (each of these leaves the lock unlocked again).
    $lock.protect-or-queue-on-recursion({ $count++ });
    $lock.protect-or-queue-on-recursion({ $count++ });
    is $count, 3, 'repeated non-recursive calls each run inline';
}

{
    my Lock::Async $lock .= new;
    my @order;
    my $inner;
    my $outer = $lock.protect-or-queue-on-recursion({
        @order.push: 'outer-start';
        $inner = $lock.protect-or-queue-on-recursion({
            @order.push: 'inner';
            'inner-value';
        });
        @order.push: 'outer-end';
    });
    nok $outer.defined, 'the outer (non-recursive) call still returns an undefined value';
    isa-ok $inner, Promise, 'the recursive inner call returns a Promise';
    is await($inner), 'inner-value', 'that Promise is kept with the queued block value';
    is @order.join(','), 'outer-start,outer-end,inner',
        'the queued block runs only after the outer call released the lock';
}

# --- Lock::Async.with-lock-hidden-from-recursion-check -------------------

{
    my Lock::Async $lock .= new;
    my $hidden-value;
    my @order;
    $lock.protect-or-queue-on-recursion({
        @order.push: 'outer-start';
        # Hidden from the recursion check, so this runs *now* rather than
        # being queued -- and hands back the block's own value.
        $hidden-value = $lock.with-lock-hidden-from-recursion-check({
            @order.push: 'hidden';
            'hidden-value';
        });
        @order.push: 'outer-end';
    });
    is $hidden-value, 'hidden-value',
        'with-lock-hidden-from-recursion-check returns the block value';
    is @order.join(','), 'outer-start,hidden,outer-end',
        'and runs the block immediately, in place';
}

{
    # Hiding is scoped to the call: after it returns, the lock is on the
    # recursion list again, so a nested call still queues.
    my Lock::Async $lock .= new;
    my $queued;
    $lock.protect-or-queue-on-recursion({
        $lock.with-lock-hidden-from-recursion-check({ 'x' });
        $queued = $lock.protect-or-queue-on-recursion({ 'queued-value' });
    });
    isa-ok $queued, Promise, 'recursion detection is restored after hiding';
    is await($queued), 'queued-value', 'and the queued block still runs';
}

{
    # Outside any protect-or-queue-on-recursion chain the method simply runs
    # the block; it never tries to take the lock itself.
    my Lock::Async $lock .= new;
    is $lock.with-lock-hidden-from-recursion-check({ 41 + 1 }), 42,
        'with-lock-hidden-from-recursion-check works outside a recursion chain';
}

# --- Process exit waits for non-app_lifetime threads ---------------------
#
# `Type/Thread.rakudoc`: with the default `:!app_lifetime`, "the process will
# only terminate when the thread has finished"; with `:app_lifetime` the thread
# "is killed when the main thread of the process terminates".

sub child-output(Str $code --> Str) {
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out;
}

{
    # The mainline ends immediately; the process must still wait for the
    # thread. The sleep is what makes the wait observable at all -- it is not
    # used to sequence anything the assertions depend on.
    my $out = child-output(
        'Thread.start({ sleep 0.5; print "waited" }); print "main;"'
    );
    is $out, 'main;waited',
        'a fire-and-forget Thread.start keeps the process alive until it finishes';
}

{
    my $out = child-output(
        'Thread.start(:app_lifetime, { sleep 10; print "late" }); print "main;"'
    );
    is $out, 'main;',
        'an :app_lifetime thread is killed at exit instead of being waited for';
}

{
    my $out = child-output(
        'Thread.new(code => { sleep 0.5; print "ran" }).run; print "main;"'
    );
    is $out, 'main;ran', 'a Thread started with .run is waited for too';
}

{
    my $out = child-output(
        'Thread.start({ sleep 10; print "late" }); print "main;"; exit 0'
    );
    is $out, 'main;', 'exit does not wait for outstanding threads';
}
