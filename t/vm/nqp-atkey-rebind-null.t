use Test;
use nqp;

plan 3;

my %table = present => 42;
ok nqp::isnull(nqp::atkey(%table, 'missing')),
    'nqp::atkey returns a native null for a missing key';

my $replacement;
$replacement := nqp::atkey(%table, 'missing');
ok nqp::isnull($replacement),
    'rebinding a missing nqp::atkey result preserves the native null';

my $assigned = 1;
$assigned = Nil;
is $assigned.raku, 'Any',
    'ordinary assignment of Nil still resets an untyped scalar to Any';
