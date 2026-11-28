use Test;

plan 5;

# A sigilless (`\v`) parameter aliases a variable handed to it, so a callee's
# write reaches the caller's variable. A typed routine (TRIR) calling such a
# callee handed over the variable's VALUE, and the callee's write died with
# "Cannot modify an immutable Int" on every call after the first (the first
# runs untyped, before the call site reaches TRIR). Each routine is called
# repeatedly so the TRIR path is the one exercised.

sub set-five(\v) { v = 5 }
sub local-scalar() { my $a = 1; set-five($a); $a }
is (local-scalar() for ^3).join(','), '5,5,5', 'a callee writes a local through a sigilless parameter';

sub bump(\v) { v = v + 1 }
sub twice() { my $n = 10; bump($n); bump($n); $n }
is (twice() for ^3).join(','), '12,12,12', 'two writes in a row both land';

sub append(\s, $x) { s = s ~ $x }
sub build() { my $s = 'a'; append($s, 'b'); append($s, 'c'); $s }
is (build() for ^3).join(','), 'abc,abc,abc', 'a sigilless parameter before an ordinary one';

# A value (not a variable) is still read-only on the callee's side.
sub try-write(\v) { v = 1; 'wrote' }
sub with-value() { try { try-write(42) } // 'refused' }
is (with-value() for ^3).join(','), 'refused,refused,refused', 'a value argument stays read-only';

# A read-only callee is unaffected.
sub peek(\v) { v ~ '!' }
sub reader() { my $x = 'hi'; peek($x) }
is (reader() for ^3).join(','), 'hi!,hi!,hi!', 'a read-only sigilless callee sees the value';
