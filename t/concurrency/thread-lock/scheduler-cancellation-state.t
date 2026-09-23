use Test;

# Test::Scheduler 1.2 constructs Cancellation handles itself and checks their
# state after cancelling scheduled virtual-time work.
plan 4;

my $cancellation = Cancellation.new;
ok $cancellation.can('cancelled'), 'Cancellation exposes cancelled';
nok $cancellation.cancelled, 'a new Cancellation is not cancelled';
$cancellation.cancel;
ok $cancellation.cancelled, 'cancel marks the Cancellation';
ok $cancellation.cancelled, 'cancelled remains true';
