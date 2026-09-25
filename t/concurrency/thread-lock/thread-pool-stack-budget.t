use Test;

# ADR-0123 / #9377: the worker pool grows only while a full-size stack fits the
# address-space budget, queues work for a running worker past it, grows
# regardless when every worker is blocked, and turns a thread the OS refuses
# into a catchable X::AdHoc instead of a panic.

plan 7;

my $exe = $*EXECUTABLE.absolute;

sub run-child(Str $code, :%env, Str :$ulimit) {
    my %e = %*ENV;
    %e{.key} = .value for %env;
    my @cmd = $ulimit
        ?? ('bash', '-c', "ulimit -v $ulimit; exec \"\$@\"", '_', $exe, '-e', $code)
        !! ($exe, '-e', $code);
    my $p = run |@cmd, :out, :err, :env(%e);
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out, $err)
}

my $burst = q:to/CODE/;
    my @p;
    for ^64 { @p.push: start { my $s = 0; $s += $_ for ^2000; $s } }
    say (await @p).elems;
    CODE

# A budget of 0 allows no optional growth at all: the first task gets the one
# worker it needs, and the other 63 queue for it instead of each reserving a
# 256 MiB stack.
{
    my ($code, $out, $err) = run-child($burst, :env(MUTSU_STACK_BUDGET_MB => 0));
    is $out.trim, '64', 'a CPU-bound burst queues for a running worker under a zero budget';
    is $code, 0, '... and exits cleanly';
}

# Every worker blocked on work that is still queued: the pool must grow past
# the budget, or this deadlocks.
{
    my $code = q:to/CODE/;
        my $gate = Promise.new;
        my @waiters = (^6).map: { start { await $gate; 1 } };
        my $opener = start { $gate.keep(True); 1 };
        say (await @waiters, $opener).elems;
        sub nest($n) { $n == 0 ?? 0 !! await start { 1 + nest($n - 1) } }
        say nest(10);
        CODE
    my ($exit, $out, $err) = run-child($code, :env(MUTSU_STACK_BUDGET_MB => 0));
    is $out.lines.join(','), '7,10', 'blocked workers still get their queued work run under a zero budget';
}

if $*KERNEL.name ne 'linux' || !"/bin/bash".IO.e {
    skip 'ulimit -v needs Linux and bash', 4;
}
else {
    # The #9377 shape: under a 3 GB address-space limit, 64 x 256 MiB stacks
    # cannot all exist. This panicked with EAGAIN in `thread_compat` before.
    my ($code, $out, $err) = run-child($burst, :ulimit<3000000>);
    is $out.trim, '64', 'a start burst completes under ulimit -v';
    unlike $err, /panicked/, '... without a panic';

    # Leave the process almost no address space beyond what it already uses,
    # so no thread stack fits: every refusal must be a catchable X::AdHoc.
    my $refuse = q:to/CODE/;
        say "/proc/self/status".IO.lines.grep(/^VmSize/).head.words[1];
        my $p = start { 42 };
        try await $p;
        say "start: ", $!.^name, ": ", $!.message if $!;
        try Thread.start({ 1 });
        say "thread: ", $!.^name, ": ", $!.message if $!;
        say "survived";
        CODE
    my ($, $calib) = run-child($refuse);
    my $vmsize = $calib.lines.head.Int;
    my ($rc, $rout, $rerr) = run-child($refuse, :ulimit(~($vmsize + 12_000)));
    ok $rout.contains('survived'), 'a refused thread does not take the process down';
    my @refusals = $rout.lines.grep(/^ ['start'|'thread'] ': '/);
    ok @refusals && @refusals.all ~~ /'X::AdHoc: Could not create a new Thread'/,
        '... and surfaces as X::AdHoc'
        or diag "stdout: $rout\nstderr: $rerr";
}
