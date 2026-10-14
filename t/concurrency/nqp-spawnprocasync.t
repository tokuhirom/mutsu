use Test;
use nqp;

# nqp::spawnprocasync / nqp::permit / nqp::killprocasync push the child's
# events onto a ConcBlockingQueue as `[callback, args...]` (#11502,
# ADR-11502 §2.4.3). Each callback here is a string naming its event. Raw
# items are read inline: a VMArray passed to a Raku sub is hllized.

plan 20;

my class Q is repr('ConcBlockingQueue') {}
my class TC is repr('AsyncTask') {}

# The child sleeps before it writes where the order against `done` is
# checked, so the permits (given right after the spawn) come first.
#
# Runs `sh -c $script` with the given callbacks and permits, collecting the
# events until `done` and the end of every permitted stream (or a 5s
# sentinel), as "name:arg|arg" strings.
sub run-sh($script, :$out, :$err, :$permit-out, :$permit-err, :$kill, :$cwd, :$env) {
    my $q := nqp::create(Q);
    my $cb := nqp::hash();
    nqp::bindkey($cb, $_, $_) for <done ready error>;
    nqp::bindkey($cb, 'stdout_bytes', 'out') if $out;
    nqp::bindkey($cb, 'stderr_bytes', 'err') if $err;
    nqp::bindkey($cb, 'buf_type', buf8.new);
    my $argv := nqp::list('sh', '-c', nqp::decont($script));
    my $environ := nqp::getenvhash();
    if $env {
        $environ := nqp::hash();
        nqp::bindkey($environ, 'FOO', nqp::decont($env));
    }
    my $h := nqp::spawnprocasync($q, '/bin/sh', $argv, nqp::decont($cwd // nqp::cwd()), $environ, $cb);
    nqp::permit($h, 1, nqp::decont($permit-out)) if $permit-out.defined;
    nqp::permit($h, 2, nqp::decont($permit-err)) if $permit-err.defined;
    nqp::killprocasync($h, 9) if $kill;
    my $sentinel := nqp::timer($q, nqp::hash(), 5000, 0, TC);
    my @events;
    # Only a stream with unlimited permits is read to its end.
    my $open = 1 + ($permit-out === -1 ?? 1 !! 0) + ($permit-err === -1 ?? 1 !! 0);
    while $open {
        my $item := nqp::shift($q);
        if nqp::ishash($item) { @events.push: 'timeout'; last }
        my $name = nqp::atpos($item, 0);
        my @args;
        for 1 ..^ nqp::elems($item) -> $i {
            my $v := nqp::atpos($item, $i);
            @args.push: !nqp::isconcrete($v) ?? 'null'
                !! $v ~~ Blob ?? $v.^name ~ '=' ~ $v.decode.raku
                !! nqp::isint($v) ?? ($name eq 'ready' && $i == 2 ?? 'pid' !! nqp::unbox_i($v))
                !! nqp::isstr($v) ?? nqp::unbox_s($v)
                !! $v.^name;
        }
        @events.push: "$name:" ~ @args.join('|');
        $open-- if $name eq 'done' || $name eq 'error';
        $open-- if $name eq 'out' | 'err' && @args[1] eq 'null';
    }
    nqp::cancel($sentinel);
    $h, @events.List
}

{
    my ($h, $events) = run-sh('sleep 0.5; echo a; echo b >&2', :out, :err, :permit-out(-1), :permit-err(-1));
    my @events = @$events;
    is $h.^name, 'BOOTIO', 'the handle is a BOOTIO';
    is $h.REPR, 'MVMOSHandle', 'of REPR MVMOSHandle';
    is @events[0], 'ready:BOOTIntArray|pid', 'ready pushes the stream fds and the pid';
    ok @events (cont) 'out:0|Buf[uint8]="a\n"|null', 'a stdout chunk is a Buf of buf_type with its sequence number';
    ok @events (cont) 'out:1|null|null', 'the end of stdout';
    ok @events (cont) 'err:0|Buf[uint8]="b\n"|null', 'a stderr chunk';
    ok @events (cont) 'err:1|null|null', 'the end of stderr';
    is @events[*-1], 'done:0', 'done comes after the output, with the wait status';
}

{
    my ($, $events) = run-sh('exit 3');
    my @events = @$events;
    is-deeply @events, ['ready:BOOTIntArray|pid', 'done:768'], 'an exit code is the wait status (code << 8)';
}

{
    my ($, $events) = run-sh('sleep 0.5; echo a; sleep 0.3; echo b', :out, :permit-out(1));
    my @events = @$events;
    is @events.grep(*.starts-with('out:')).elems, 1, 'a limited permit reads that many chunks';
    is @events[*-1], 'done:0', 'and done still comes';
}

{
    my ($, $events) = run-sh('echo unread', :out);
    my @events = @$events;
    is-deeply @events, ['ready:BOOTIntArray|pid', 'done:0'], 'an unpermitted stream is not read';
}

{
    my ($, $events) = run-sh('sleep 5', :kill);
    my @events = @$events;
    is @events[*-1], 'done:9', 'killprocasync signals the child';
}

{
    # A pipe read is not tied to the child's write boundaries, so a reader
    # scheduled mid-write (likely under `prove -j`) may split even a single
    # write into several `out` chunks (#12931, #13265): compare the
    # concatenated payloads, not one chunk.
    my ($, $events) = run-sh('printf "%s\n%s\n" "$(pwd)" "$FOO"', :out, :permit-out(-1), :cwd</>, :env<bar>);
    my @events = @$events;
    my $payload = @events.map({ .match(/^ 'out:' \d+ '|Buf[uint8]="' (.*) '"|null' $/) ?? ~$0 !! Empty }).join;
    is $payload, '/\nbar\n', 'the child gets the cwd and environment given';
}

{
    my $q := nqp::create(Q);
    my $cb := nqp::hash();
    nqp::bindkey($cb, 'error', 'error');
    my $h := nqp::spawnprocasync($q, '/nonexistent/program', nqp::list('x'), nqp::cwd(), nqp::getenvhash(), $cb);
    is $h.^name, 'BOOTIO', 'a failed spawn still answers a handle';
    my $item := nqp::shift($q);
    is nqp::atpos($item, 0), 'error', 'and pushes the error callback';
    is nqp::unbox_s(nqp::atpos($item, 1)),
        'Failed to spawn process /nonexistent/program: no such file or directory (error code -2)',
        'with the message';
    is nqp::unbox_i(nqp::atpos($item, 2)), -2, 'and the error code';
}

throws-like { nqp::permit(42, 1, -1) }, Exception, message => 'Can only permit an AsyncTask handle',
    'permit refuses a non-handle';
throws-like { nqp::killprocasync(42, 9) }, Exception, message => 'killprocasync requires a process handle',
    'killprocasync refuses a non-handle';
