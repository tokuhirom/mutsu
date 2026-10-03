# A role attribute's custom trait runs, and every composing class gets its own
# copy of the trait-mutated Attribute, whose `compose($class)` hook runs with
# that class -- early enough for an accessor it installs to satisfy a role's
# `method x {...}` stub. Found via PDF::Class (PDF::COS::Tie's `is entry`).
use Test;
use lib 't/lib';

plan 6;

use RoleBodyUse::Tie;

role Stubby { method type {...} }
role R does Stubby { has $.t is aka<type> }

class B does R { }
is B.new(:t(7)).type, 'alias of t: 7', 'the trait compose hook satisfies a role stub';
is B.^attributes.first(*.name eq '$!t').package.^name, 'B',
    'the composed attribute belongs to the composing class';

class B2 does R { }
is B2.new(:t(8)).type, 'alias of t: 8', 'a second composing class gets the alias too';

role Plain { has $.v is aka<value> }
class P does Plain { }
is P.new(:v(1)).value, 'alias of v: 1', 'a role attribute trait without a stub';

my @seen;
multi trait_mod:<is>(Attribute $a, :$seen!) { @seen.push: $a.name }
role Once { has $.o is seen }
class O1 does Once { }
class O2 does Once { }
is-deeply @seen, ['$!o'], 'a role attribute trait runs once, not per composition';

class K { has $.k is aka<kay> }
is K.new(:k(2)).kay, 'alias of k: 2', 'a class attribute trait still works';
