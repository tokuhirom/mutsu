use v6;
use Test;

plan 5;

# The core bare calls (`callsame`, `nextsame`, `nextcallee`, `lastcall`) are
# resolved by the bareword classifier and lowered where the parser built them
# (ADR-15760 slice 2).

class Base { method who { 'base' } }
class Derived is Base { method who { 'derived+' ~ callsame } }
is Derived.new.who, 'derived+base', 'a bare callsame redispatches';

multi sub pick(Int $x) { 'int' ~ nextsame }
multi sub pick(Any $x) { 'any' }
is pick(1), 'any', 'a bare nextsame goes to the next candidate (and does not return)';

class D2 is Base { method who { my &next = nextcallee; 'd2+' ~ next(self) } }
is D2.new.who, 'd2+base', 'nextcallee answers the next method';

multi sub only(Int $x) { lastcall; 'last' }
is only(3), 'last', 'a bare lastcall is a no-op call';

sub callsame-shadow { 'declared' }
is callsame-shadow, 'declared', 'an ordinary routine named like them is untouched';
