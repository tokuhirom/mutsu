use experimental :rakuast;
use Test;

# An unspace between a declaration and a dotted subscript is outside the
# subscript's source range. Rakudo 2026.11 places the brackets and their
# contents even when the unspace has more than one following blank.
plan 9;

sub range($node) { $node.origin.from ~ '-' ~ $node.origin.to }

my $one = Q[my @b\ .[0] = 5].AST.statements[0].expression.postfix;
is range($one), '8-15', 'array subscript starts at the opening bracket';
is range($one.index), '9-10', 'array index has its own range';
is range($one.index.statements[0]), '9-10', 'index statement has its range';

my $two = Q[my @b\  .[1] = 3].AST.statements[0].expression.postfix;
is range($two), '9-16', 'multiple blanks after unspace are skipped';
is range($two.index), '10-11', 'the index after multiple blanks is placed';
is range($two.index.statements[0]), '10-11', 'its statement is placed';

my $hash = Q[my %g\  .<z> = 2].AST.statements[0].expression.postfix;
is range($hash), '9-16', 'literal hash subscript starts at its bracket';
is range($hash.index), '10-11', 'its word quote has a range';
is range($hash.index.segments[0]), '10-11', 'its literal segment has a range';
