use Test;

# #9401 (ADR-0123 "Remaining panics"): the default-stack service threads --
# the timer driver (`Promise.in`, `Supply.interval`, scheduler `cue(:in)`),
# the signal reader, `IO::Path.watch`'s poller, the async-socket accept pump,
# `Promise.allof`'s waiter and `Proc::Async.start`'s helpers -- used to panic
# the whole process when the OS refused them. A refusal is now the same
# catchable X::AdHoc a refused user thread raises: thrown by the call, or, for
# `Proc::Async.start`, carried by its broken promise.
#
# An OS limit cannot refuse these threads deterministically (the process's own
# address-space use moves between runs), so the refusal is injected with
# `MUTSU_REFUSE_SERVICE_THREADS`.

plan 6;

my $exe = $*EXECUTABLE.absolute;

sub run-child(Str $code, Str $refuse) {
    my %e = %*ENV;
    %e<MUTSU_REFUSE_SERVICE_THREADS> = $refuse;
    my $p = run $exe, '-e', $code, :out, :err, :env(%e);
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out, $err)
}

my $all = q:to/CODE/;
    sub report($what, &code) {
        code();
        say "$what: ok";
        CATCH { default { say "$what: ", .^name, ": ", .message } }
    }
    report 'Promise.in',     { await Promise.in(0.01) };
    report 'Promise.at',     { await Promise.at(now + 0.01) };
    report 'Promise.allof',  { await Promise.allof(Promise.kept(1)) };
    report 'interval',       { Supply.interval(0.01) };
    report 'cue :in',        { $*SCHEDULER.cue({ 1 }, :in(0.01)) };
    report 'cue :every',     { $*SCHEDULER.cue({ 1 }, :every(0.01)) };
    report 'signal',         { signal(SIGUSR1) };
    report 'watch',          { ".".IO.watch };
    report 'listen',         { IO::Socket::Async.listen('127.0.0.1', 0).tap({ ; }) };
    report 'Proc::Async',    { await Proc::Async.new($*EXECUTABLE, '-e', 'say 1').start };
    say "survived";
    CODE

{
    my ($rc, $out, $err) = run-child($all, 'all');
    is $rc, 0, 'refusing every service thread does not take the process down';
    unlike $err, /panicked/, '... and is never a panic';
    my @lines = $out.lines.grep(/':'/);
    is +@lines, 10, '... every operation reports';
    ok @lines.all ~~ /': X::AdHoc: Could not create a new Thread'/,
        '... each as the catchable refused-thread X::AdHoc'
        or diag $out;
}

# A reader refused from inside the `proc-wait` helper cannot throw to the
# caller: the child is killed and the promise the caller holds is broken.
{
    my $code = q:to/CODE/;
        my $p = Proc::Async.new($*EXECUTABLE, '-e', 'say 1; sleep 30');
        $p.stdout.tap({ ; });
        my $t = now;
        await $p.start;
        CATCH { default { say .^name, ": ", .message; say "fast" if now - $t < 20 } }
        CODE
    my ($rc, $out, $err) = run-child($code, 'proc-out');
    like $out, /^ 'X::AdHoc: Could not create a new Thread'/,
        'a refused stdout reader breaks the start promise';
    ok $out.contains('fast'), '... after killing the child rather than waiting for it';
}
