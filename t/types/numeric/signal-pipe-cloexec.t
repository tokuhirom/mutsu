use Test;

plan 2;

# Only fds above stderr: 0-2 legitimately come from the parent's own stdio.
my $list = 'for f in /proc/$$/fd/*; do n=${f##*/}; [ "$n" -gt 2 ] && readlink "$f"; done; true';

# The signal self-pipe must not leak into child processes (#11220).
signal(SIGTERM).tap({ });
my @fds = run('sh', '-c', $list, :out).out.slurp(:close).lines;
is @fds.grep(*.starts-with('pipe:')).elems, 0,
    'child does not inherit the signal pipe';

# The :merge pipe must not leak extra ends into the child either.
my @m = run('sh', '-c', $list, :out, :merge).out.slurp(:close).lines;
is @m.grep(*.starts-with('pipe:')).elems, 0,
    'merged child does not inherit the :merge pipe ends';
