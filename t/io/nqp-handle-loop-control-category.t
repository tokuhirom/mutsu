use Test;
use nqp;

# From Sequence::Generator (t/02-exhaustive): a generator that calls `last` ends
# the sequence through `nqp::handle(..., 'LAST', ...)`; the control exception must
# not leave through to the caller's loop.
plan 7;

my &g = { last if $_ >= 5; $_ + 1 };
my @log;
sub pull($x) { nqp::handle(g($x), 'LAST', @log.push('ended')) }

my @seen;
for 1 {
    @seen.push(pull(1));
    pull(9);
    @seen.push('after');
}
is @seen.grep(Int|Str).join(','), '2,after', "'LAST' handler takes the last, the loop goes on";
is @log.join(','), 'ended', 'and ran';

my $skipped = 0;
sub nexter($x) { nqp::handle(($x ?? (next) !! 7), 'NEXT', $skipped++) }
nexter(1);
is $skipped, 1, "'NEXT' handler takes a next";
is nexter(0), 7, 'no control exception: the body value comes back';

sub errs { nqp::handle(die("boom"), 'LAST', 'wrongly caught') }
throws-like { errs() }, X::AdHoc, "'LAST' does not take errors";

sub catcher { nqp::handle(die("boom"), 'CATCH', 'caught') }
is catcher(), 'caught', "'CATCH' still takes errors";

sub ret { nqp::handle(g(9), 'LAST', (return 'returned')); 'not reached' }
is ret(), 'returned', 'the handler can return from the routine';
