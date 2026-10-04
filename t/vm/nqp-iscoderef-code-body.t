use v6;
use Test;
use nqp;

# MVMCode is the executable body held by Code.$!do. A high-level routine
# remains P6opaque even though both values are callable.
plan 8;

is nqp::iscoderef(sub { }), 0, 'a high-level Sub is not a code ref';
is nqp::iscoderef(Code), 0, 'the high-level Code type is not a code ref';
is nqp::iscoderef(42), 0, 'an Int is not a code ref';

my $routine := sub { 42 };
my $body := nqp::getattr($routine, Code, '$!do');
is nqp::iscoderef($routine), 0, 'the owning routine stays high-level';
is nqp::iscoderef($body), 1, 'Code.$!do is a direct code ref';
my $alias := $body;
is nqp::iscoderef($alias), 1, 'the code body keeps its identity through binding';
my $fresh := nqp::freshcoderef($body);
is nqp::iscoderef($fresh), 1, 'freshcoderef retains the raw code representation';
nqp::setcodename($body, 'renamed');
is nqp::iscoderef($body), 1, 'renaming does not change the code body representation';
