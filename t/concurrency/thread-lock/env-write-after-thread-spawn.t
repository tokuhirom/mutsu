use Test;

# Regression pin for todo/deep/env-var-write-invisible-to-spawn-after-a-thread.md:
# once ANY OS thread has ever been spawned via mutsu's clone_for_thread /
# worker_pool machinery (live taps: Proc::Async, Supply.interval, sockets,
# signals), a later `%*ENV<key> = val` write must still be visible to a
# SUBSEQUENTLY spawned child that relies on default OS-level env inheritance
# (no explicit :ENV/:env passed to run/shell/Proc::Async).
#
# `%*ENV` writes also call std::env::set_var, which is documented as
# UB-hazardous once other threads exist -- so `run()`/`shell()`/`Proc::Async`
# stop relying on that inheritance and explicitly rebuild the child's
# environment from mutsu's own %*ENV hash instead. This test exercises two
# independent ways to get a second OS thread running (Proc::Async and
# Supply.interval) and confirms both `run()` and `shell()` still see a
# %*ENV write made afterward.

plan 4;

sub spawn-a-thread-via-proc-async() {
    my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'say "hi"';
    my $stdout = '';
    $p.stdout.tap: { $stdout ~= $_ };
    my $prom = $p.start;
    await $prom;
}

sub spawn-a-thread-via-supply-interval() {
    my $done = Promise.new;
    my $count = 0;
    my $tap = Supply.interval(0.05).tap({ $count++; $done.keep if $count >= 2 });
    await Promise.anyof($done, Promise.in(2));
    $tap.close;
}

{
    spawn-a-thread-via-proc-async();
    temp %*ENV<MUTSU_ENV_AFTER_THREAD_TEST> = 'meows-async';
    is run('sh', '-c', 'echo $MUTSU_ENV_AFTER_THREAD_TEST', :out).out.slurp(:close).trim,
        'meows-async',
        'run() sees a %*ENV write made after a Proc::Async thread was spawned';
    is shell('echo $MUTSU_ENV_AFTER_THREAD_TEST', :out).out.slurp(:close).trim,
        'meows-async',
        'shell() sees a %*ENV write made after a Proc::Async thread was spawned';
}

{
    spawn-a-thread-via-supply-interval();
    temp %*ENV<MUTSU_ENV_AFTER_THREAD_TEST> = 'meows-supply';
    is run('sh', '-c', 'echo $MUTSU_ENV_AFTER_THREAD_TEST', :out).out.slurp(:close).trim,
        'meows-supply',
        'run() sees a %*ENV write made after a Supply.interval thread was spawned';
    is shell('echo $MUTSU_ENV_AFTER_THREAD_TEST', :out).out.slurp(:close).trim,
        'meows-supply',
        'shell() sees a %*ENV write made after a Supply.interval thread was spawned';
}
