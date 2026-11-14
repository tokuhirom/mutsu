use v6;
use Test;

# `Proc::Async.new(:pty(:rows, :cols))` runs the child behind a pseudo-
# terminal: it sees a terminal of that size on stdin/stdout/stderr, its
# output (stderr merged) arrives on `.stdout`, writes reach it without `:w`,
# and `.resize-pty` changes the size it sees. Same as raku (#17751).

plan 9;

sub collect(Proc::Async $p, &during = -> {}) {
    my $out = '';
    $p.stdout.tap(-> $chunk { $out ~= $chunk });
    my $started = $p.start;
    await $p.ready;
    during();
    my $proc = await $started;
    ($out, $proc)
}

my $p = Proc::Async.new(:pty(:cols(80), :rows(24)),
    $*EXECUTABLE, '-e', 'say $*OUT.t ?? "PTY-YES" !! "PTY-NO"');
ok $p.pty, '.pty is True';
my ($out, $proc) = collect($p);
is $out, "PTY-YES\n", 'the child sees a terminal';
is $proc.exitcode, 0, 'its exit code';

($out,) = collect(Proc::Async.new(:pty(:rows(5), :cols(20)),
    'sh', '-c', 'stty size; echo err >&2'));
is $out, "5 20\nerr\n", 'the requested size, and stderr merged into stdout';

$p = Proc::Async.new(:pty(:rows(5), :cols(20)), 'sh', '-c', 'read x; echo got:$x');
($out,) = collect($p, { $p.print("hello\n") });
is $out, "hello\ngot:hello\n", 'a write without :w reaches the child (echoed by the terminal)';

$p = Proc::Async.new(:pty(:rows(5), :cols(20)), 'sh', '-c', 'sleep 0.5; stty size');
($out,) = collect($p, { $p.resize-pty(:rows(7), :cols(30)) });
is $out, "7 30\n", '.resize-pty changes the size the child sees';

($out, $proc) = collect(Proc::Async.new(:pty(:rows(5), :cols(20)), 'sh', '-c', 'exit 3'));
is $proc.exitcode, 3, 'a non-zero exit code';

nok Proc::Async.new(:!pty, 'true').pty, ':!pty is no pty';
throws-like { Proc::Async.new(:pty, 'true') }, Exception,
    message => ':pty must be set as follows: :pty(:cols(12), :rows(34))',
    'a bare :pty is refused';
