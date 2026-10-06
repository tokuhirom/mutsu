unit module AtomicRwTarget;

# A module's own file-scope variables handed to an `is rw` parameter from the
# module's routines (#12007): the atomicint is a native container, the plain
# scalar is not.
my atomicint $hits = 0;
my $plain = 0;

sub bump-rw($p is rw) { $p⚛++ }

sub bump-hits() is export { bump-rw($hits); $hits }
sub bump-plain() is export { bump-rw($plain) }
sub plain-value() is export { $plain }
