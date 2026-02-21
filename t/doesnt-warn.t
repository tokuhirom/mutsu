use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 4;

# Code without warnings passes
doesn't-warn 'say 42', 'say does not warn';

# Simple expression without warnings
doesn't-warn '1 + 2', 'arithmetic does not warn';

# Multiple statements without warnings
doesn't-warn 'my $x = 1; my $y = 2; $x + $y', 'multiple stmts no warn';

# Note goes to stderr but is not a warning
doesn't-warn 'note "info"', 'note is not a warn';
