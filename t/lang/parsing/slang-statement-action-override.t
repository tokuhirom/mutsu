use Test;
use lib 't/lib';
BEGIN { plan 4 }

# A slang's actions role may override the core `statement` action: it sees
# each statement's `$/` placed in the unit's text (`.orig`, `.from`, `.to`)
# and `$*ORIGIN-SOURCE`, and the node it attaches replaces the statement.
# Dawa wraps every statement in a call of its debugger's stop point this way.

use StatementActionSlang;

my $x = 1;
$x += 41;

is $x, 42, 'the wrapped statements still run';
my @seen = seen-statements();
is-deeply @seen.head(4).List,
    ('12: my $x = 1 (slang-statement-action-override.t)',
     '13: $x += 41 (slang-statement-action-override.t)',
     "15: is \$x, 42, 'the wrapped statements still run' (slang-statement-action-override.t)",
     '16: my @seen = seen-statements() (slang-statement-action-override.t)'),
    'each statement after the use is recorded with its line, text and file';
is seen-statements().grep(*.starts-with('17: ')).elems, 1, 'a statement spanning lines is recorded once';
is (1 + 1), 2, 'a wrapped expression keeps its value';
