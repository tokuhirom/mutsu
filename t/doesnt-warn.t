use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 4;

# Code without warnings passes
doesn't-warn 'say 42', 'say does not warn';

# Simple expression whose value is consumed does not warn.
# (A bare `1 + 2` in sink context DOES warn: "Useless use of ... in sink context".)
doesn't-warn 'my $z = 1 + 2', 'consumed arithmetic does not warn';

# Multiple statements without warnings (final value is consumed)
doesn't-warn 'my $x = 1; my $y = 2; my $z = $x + $y', 'multiple stmts no warn';

# Note goes to stderr but is not a warning
doesn't-warn 'note "info"', 'note is not a warn';
