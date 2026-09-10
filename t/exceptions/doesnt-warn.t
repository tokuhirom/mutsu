use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 7;

# Code without warnings passes
doesn't-warn 'say 42', 'say does not warn';

# A `:=` bind of a container variable is a binding (a side effect), not a
# useless value. The array-bind desugaring leaves a trailing `@a` result
# expression in its SyntheticBlock; sink analysis must not flag it (it used
# to spuriously warn "Useless use of $@a in sink context").
doesn't-warn 'my @b = (1, 2, 3); my @a := @b', 'my array := bind does not warn';
doesn't-warn 'our @b = (1, 2, 3); our @a := @b', 'our array := bind does not warn';
doesn't-warn 'my %b = (x => 1); my %a := %b', 'my hash := bind does not warn';

# Simple expression whose value is consumed does not warn.
# (A bare `1 + 2` in sink context DOES warn: "Useless use of ... in sink context".)
doesn't-warn 'my $z = 1 + 2', 'consumed arithmetic does not warn';

# Multiple statements without warnings (final value is consumed)
doesn't-warn 'my $x = 1; my $y = 2; my $z = $x + $y', 'multiple stmts no warn';

# Note goes to stderr but is not a warning
doesn't-warn 'note "info"', 'note is not a warn';
