use Test;

plan 5;

# A statement `X,;` is a one-element list, exactly as `X,` before `}` is.
sub one($n) { $n,; }
is-deeply one("n"), ("n",), 'a trailing comma before ; makes a one-element list';
sub five { 5,; }
is-deeply five(), (5,), 'a literal with a trailing comma before ;';

# The ternary else-branch must not swallow the list comma.
sub pick($o) { my $name = "n"; $o ?? ('ONLY', $name) !! $name,; }
is-deeply pick(0), ("n",), 'else branch with trailing comma is a one-element list';
is-deeply pick(1), (("ONLY", "n"),), 'then branch is the element of the list';
sub plain($n) { $n; }
is plain("n"), "n", 'no trailing comma stays a scalar';
