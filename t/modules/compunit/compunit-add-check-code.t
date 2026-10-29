use lib 't/lib';
use Test;

# `$*CU.add-check-code` (an undocumented rakudo method, maintainer-approved in
# #15513) queues a closure on the unit being compiled; the unit runs the queue
# at its CHECK phase, after its CHECK phasers and before INIT. `static::CLI`
# registers its analysis this way from its `sub EXPORT`.
use CheckCodeHook;

plan 9;

BEGIN @CHECK-LOG.push("begin");
CHECK @CHECK-LOG.push("CHECK phaser");
INIT  @CHECK-LOG.push("INIT phaser");

is-deeply @CHECK-LOG.List,
    ("export on RakuAST::CompUnit", "begin", "CHECK phaser",
     "second check code, 0 args, same cu: True", "first check code", "INIT phaser").List,
    'queued check code runs after CHECK phasers, last queued first, before INIT';

ok RakuAST::CompUnit.^can('add-check-code'), 'RakuAST::CompUnit can add-check-code';

# a unit that finished compiling takes the call without error and never runs it
my $cu;
BEGIN $cu = $*CU;
my $late = 'not run';
lives-ok { $cu.add-check-code({ $late = 'ran' }) }, 'add-check-code after compile is accepted';
is $late, 'not run', '... and the closure never runs';

# a closure may read the unit it was queued on
my $seen;
BEGIN $*CU.add-check-code({ $seen = 'ran' });
is $seen, 'ran', 'a BEGIN-queued closure ran before the mainline';
isa-ok $cu, RakuAST::CompUnit, '$*CU is a CompUnit';
is @CHECK-LOG.elems, 6, 'every phase logged once';

# `static::CLI` guards its input with `$cu ~~ RakuAST::CompUnit:D`.
ok $cu ~~ RakuAST::CompUnit:D, 'a CompUnit matches RakuAST::CompUnit:D';
nok RakuAST::CompUnit ~~ RakuAST::CompUnit:D, 'the type object does not';
