use Test;

plan 8;

# A bare block in tail position that reaches a `when`/`default` ends at the
# matched clause with the clause's value, as in statement position; it does not
# unwind out of the routine or the mainline.
my @log;
sub only-block { { when True { @log.push('when') }; @log.push('never') } }
only-block();
is-deeply @log, ['when'], 'sub whose body is one bare block ends at the matched when';

sub block-then-value { { when True { @log.push('w2') }; @log.push('no') }; 5 }
is block-then-value(), 5, 'the sub continues after the block';

sub tail-value { { when False { 'x' }; 'tail' } }
is tail-value(), 'tail', 'a failed when falls through to the block tail';

sub when-value { { when True { 'val' } } }
is when-value(), 'val', 'the matched clause value is the routine value';

is (do { { when True { 'v' } } }), 'v', 'do { { when ... } }';

my $c = -> { { when True { @log.push('cl') }; @log.push('no2') } };
$c();
is-deeply @log[*-1], 'cl', 'a pointy block whose body is a bare block';

is-deeply @log.grep(* eq 'never' | 'no' | 'no2').elems, 0, 'nothing after the matched when ran';

use MONKEY-SEE-NO-EVAL;
is EVAL('{ when True { "e" }; "no" }'), 'e', 'a unit whose last statement is such a block';
