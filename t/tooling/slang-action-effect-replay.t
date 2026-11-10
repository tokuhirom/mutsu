use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang production's action may do more than attach a node: it can write to
# state of the slang's own module (Qwiratry::Mold::Slang's `mold` registers into a
# module-level registry). That state is the importing unit's, so the action runs
# where the unit can see its effects, once per production (#17243, ADR-17243).

plan 2;

use ActionEffectSlang;

remember alpha;
remember beta;

# A production inside a loop is one production: rakudo runs its action once, at
# compile time, and so does the replay.
for 1..3 { remember gamma }

sub later() { remember delta }
later(); later();

is-deeply collected().grep(* ne 'compiled').Array, ['alpha', 'beta', 'gamma', 'delta'],
    'each production registered once, a loop body and a routine included';

# A routine an action compiles may name a variable of the importing unit.
my $outer;
compile-outer;
is-deeply collected().tail, 'compiled', 'the host does not judge the unit\'s variables undeclared';
