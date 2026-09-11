use v6;
use Test;

# Pin for issue #7609: a `whenever <Promise>` inside `react`/`supply` must
# deliver its result through the promise's own waiter list, not through a
# dedicated OS thread parked in a blocking wait.
#
# Before this, every `whenever $promise` spawned one `promise-wait` thread
# that lived until the promise settled -- even after the enclosing react had
# already finished via `done`. `roast/S17-procasync/stress.t`'s 1200 reacts,
# each with a `whenever Promise.in(5)`, peaked at 420 concurrent threads and
# 4.5 GB of address space for that reason alone.

plan 4;

# Each react ends immediately (the first whenever's promise is already kept),
# but leaves a second whenever watching a promise that cannot fire during this
# test. None of those may hold a thread.
for ^100 {
    my $ready = Promise.new;
    $ready.keep(1);
    react {
        whenever $ready { done }
        whenever Promise.in(600) { }
    }
}

my $threads = "/proc/$*PID/status".IO.lines.first(*.starts-with('Threads:'));
my $n = $threads.words[1].Int;
ok $n < 40, "100 abandoned `whenever <Promise>` sources hold no threads (got $n)"
    or diag "Threads: $n -- a `whenever <Promise>` is parking an OS thread again";

# The delivery itself must still work, for both outcomes.
my $kept = Promise.new;
start { sleep 0.05; $kept.keep(42) }
my $got;
react {
    whenever $kept -> $v { $got = $v; done }
    whenever Promise.in(5) { done }
}
is $got, 42, 'a kept promise still reaches its whenever body';

my $broken = Promise.new;
start { sleep 0.05; $broken.break('nope') }
my $died;
try {
    react {
        whenever $broken { flunk 'a broken promise must not run the whenever body' }
        whenever Promise.in(5) { done }
    }
    CATCH { default { $died = $_ } }
}
ok $died.defined && $died.message.contains('nope'),
    'a broken promise still dies the react block';

# A promise already resolved when the whenever is registered still fires.
my $pre = Promise.new;
$pre.keep('early');
my $seen;
react {
    whenever $pre -> $v { $seen = $v; done }
    whenever Promise.in(5) { done }
}
is $seen, 'early', 'an already-kept promise fires its whenever body';
