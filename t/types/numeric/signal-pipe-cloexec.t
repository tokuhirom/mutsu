use Test;

plan 2;

# Only fds above stderr: 0-2 legitimately come from the parent's own stdio.
my $list = 'for f in /proc/$$/fd/*; do n=${f##*/}; [ "$n" -gt 2 ] && readlink "$f"; done; true';
sub child-pipes(*%o) { run('sh', '-c', $list, :out, |%o).out.slurp(:close).lines.grep(*.starts-with('pipe:')).sort.Array }

# A harness (prove -j, make's jobserver) may hand us pipes of its own; those are
# inherited legitimately, so compare against what a child sees before we create any.
my @baseline = child-pipes;

# The signal self-pipe must not leak into child processes (#11220).
signal(SIGTERM).tap({ });
is-deeply child-pipes, @baseline, 'child does not inherit the signal pipe';

# The :merge pipe must not leak extra ends into the child either.
is-deeply child-pipes(:merge), @baseline, 'merged child does not inherit the :merge pipe ends';
