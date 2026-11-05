use Test;
use lib $*PROGRAM.parent(2).add('lib-class-body-bound-aggregate');

# A `unit class` file-scope `my %h := ...` / `my @a := ...` is the class's own
# static. Two modules declaring a bound `@`/`%` of the same name used to share
# the first-loaded module's container, because the bound declaration (wrapped
# in a parser SyntheticBlock) was missing from the class's declared statics.
# Net::BGP's Notify::Header / Notify::Open `%error-subcodes` hit this and
# answered a bad marker with the wrong NOTIFICATION.

plan 8;

use BoundAggA;
use BoundAggB;

BoundAggA.add("a1");
BoundAggA.add("a2");
BoundAggB.add("b1");

is BoundAggA.keys-seen, "a1,a2", 'A sees only its own hash entries';
is BoundAggB.keys-seen, "b1", 'B sees only its own hash entries';
is BoundAggA.log-count, 2, 'A has its own array';
is BoundAggB.log-count, 1, 'B has its own array';
isnt BoundAggA.id, BoundAggB.id, 'the two hashes are distinct objects';

BoundAggB.add("b2");
is BoundAggA.keys-seen, "a1,a2", 'A is unaffected by a later write through B';
is BoundAggB.keys-seen, "b1,b2", 'B grew';
is BoundAggA.log-count, 2, 'A array unaffected';
