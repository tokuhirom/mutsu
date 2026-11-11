use Test;
use nqp;

# rakudo's `make` is `nqp::bindattr($/, Match, '$!made', $value)` on a match
# that does `NQPMatchRole`; EEL 0.3's own exported `&make` spells it out
# (#16888).

plan 5;

'a' ~~ /a/;
is nqp::istype($/, NQPMatchRole), 1, 'a Match does NQPMatchRole';
nqp::bindattr(nqp::decont($/), Match, '$!made', 5);
is $/.made, 5, 'binding $!made is make';
grammar G { token TOP { a } }
class Actions { method TOP($/) { nqp::bindattr(nqp::decont($/), Match, '$!made', 42) } }
is G.parse('a', :actions(Actions)).made, 42, 'in a grammar action';
is Rakudo::Internals.IS-ISOLATED-MATCH($/), 0, 'a plain $/ is no isolated match';
is nqp::ctxcallerskipthunks(nqp::ctx()).^name, nqp::ctxcaller(nqp::ctx()).^name,
    'ctxcallerskipthunks is the caller';
