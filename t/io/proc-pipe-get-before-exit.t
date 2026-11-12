use Test;

# `.get` on a running child's `:out` pipe answers the next line without
# waiting for the child to exit: a child that writes a line and then waits on
# its stdin must not deadlock the parent (#17513: Red's lock-holder helper
# prints "locked" and blocks on `$*IN.get`).

plan 6;

my $proc = run $*EXECUTABLE, '-e',
    'say "first"; say "second"; $*OUT.flush; my $in = $*IN.get; say "got $in"; say "last"',
    :in, :out, :err;
is $proc.out.get, 'first', 'the first line, while the child waits';
is $proc.out.get, 'second', 'the next line';
$proc.in.say('ping');
$proc.in.close;
is $proc.out.get, 'got ping', 'a line written after the parent answered';
is $proc.out.slurp-rest, "last\n", 'slurp-rest returns what get did not read';
$proc.err.slurp-rest;
is $proc.exitcode, 0, 'the child exits cleanly';

my $done = run $*EXECUTABLE, '-e', 'print "a\nb"', :out;
is-deeply ($done.out.get, $done.out.get, $done.out.get), ('a', 'b', Nil),
    'the last line has no newline, and get after the end is Nil';
