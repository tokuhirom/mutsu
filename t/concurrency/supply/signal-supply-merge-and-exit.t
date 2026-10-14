use Test;

# #13472: `signal(A).merge(signal B)` must stay live (it used to be `done` at
# once), and `exit` inside a `whenever` of a signal supply must end the process
# (the react drive loop kept waiting for events after `exit` had latched).
# Each case runs a child, waits until it prints `started`, signals it, and
# checks its output and exit status.

plan 6;

sub run-child(Str $source) {
    my $proc = Proc::Async.new($*EXECUTABLE, '-e', $source);
    my $ready = Promise.new;
    my $out = '';
    $proc.stdout.tap(-> $s { $out ~= $s; $ready.keep if $out.contains("started") && $ready.status ~~ Planned });
    my $done = $proc.start;
    await Promise.anyof($ready, $done, Promise.in(30));
    my $was-ready = $ready.status ~~ Kept;
    $proc.kill(SIGTERM) if $was-ready;
    my $result = await Promise.anyof($done, Promise.in(30)).then({ $done.status ~~ Kept ?? $done.result !! Nil });
    $proc.kill(SIGKILL) unless $done.status ~~ Kept;
    ($was-ready, $result, $out)
}

{
    my ($ready, $result, $out) = run-child(q:to/CODE/);
        react {
            whenever signal(SIGTERM) { say 'pass'; exit 3 };
            whenever Promise.kept { say 'started'; $*OUT.flush }
        };
        CODE
    ok $ready, 'exit-in-whenever: the child started';
    is $result.exitcode, 3, 'exit inside a signal whenever ends the process with its status';
}

{
    my ($ready, $result, $out) = run-child(q:to/CODE/);
        react {
            whenever signal(SIGTERM).merge(signal SIGINT) { say 'pass'; done };
            whenever Promise.kept { say 'started'; $*OUT.flush }
        };
        say 'after';
        CODE
    ok $ready, 'merge: the merged signal supply did not complete before the signal';
    is $out.lines.List, <started pass after>, 'merge: the merged supply delivers the signal, then the react ends';
}

{
    my ($ready, $result, $out) = run-child(q:to/CODE/);
        react {
            whenever signal(SIGTERM).merge(signal SIGINT) { say 'pass'; exit 0 };
            whenever Promise.kept { say 'started'; $*OUT.flush }
        };
        CODE
    ok $ready, 'merge + exit: the child started';
    is $result.exitcode, 0, 'merge + exit: the process exits 0 on SIGTERM';
}

