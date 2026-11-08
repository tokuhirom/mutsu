use lib 't/lib';
use Test;
use nqp;

# `nqp::getattr($*CU, RakuAST::CompUnit, '$!check-phasers')` is the unit's raw
# list of CHECK closures: a module's `sub EXPORT` can push onto it, and the
# closure runs at the importing unit's CHECK phase, after its CHECK phasers
# and before INIT.
use CheckPhasersHook;

plan 2;

BEGIN @PUSHED-LOG.push("begin");
CHECK @PUSHED-LOG.push("CHECK phaser");
INIT  @PUSHED-LOG.push("INIT phaser");

is-deeply @PUSHED-LOG.List,
    ("begin", "CHECK phaser", "pushed check phaser", "INIT phaser").List,
    'a closure pushed onto $!check-phasers runs in the CHECK phase';

my $inside;
BEGIN $inside = nqp::getattr($*CU, RakuAST::CompUnit, '$!check-phasers');
ok nqp::isconcrete($inside), 'the attribute reads as a defined list while compiling';
