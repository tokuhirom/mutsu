use Test;
use lib 't/lib';
use UCPoint;
use URPort;

# Six general class/role/regex fixes surfaced while loading the Bailador web
# framework. Each was a divergence from real raku.

plan 10;

# 1. `unit class` with top-level `has`/`method` (rest of the unit is the body).
is UCPoint.new(x => 10, y => 20).s, 30, 'unit class with top-level has + method works';

# 1b. `unit role` with top-level `has`, consumed by a class.
my $c2 = (class :: does URPort {}).new(port => 9000);
is $c2.show, '0.0.0.0:9000', 'unit role with top-level has, consumed by a class';

# 2. A yada-stub method in a CLASS is an abstract stub (NOT a role requirement):
#    the class defines fine; subclasses override; calling the stub dies.
lives-ok { EVAL 'class A { method handle($m) { ... } }; True' },
    'class with a yada-stub method defines without X::Role::Composition error';
is (EVAL 'class A2 { method h($m){...} }; class B2 is A2 { method h($m){"B:$m"} }; B2.new.h("x")'),
    'B:x', 'subclass overrides the class stub method';
dies-ok { EVAL 'class A3 { method h { ... } }; A3.new.h' },
    'calling an unoverridden class stub dies (Stub code executed)';

# 2b. A required method from a ROLE is still enforced.
dies-ok { EVAL 'role R4 { method need {...} }; class C4 does R4 {}; True' },
    'role required method is still enforced (X::Role::Composition::Unimplemented)';

# 3. A double-quote inside a negated char class `<-["]>` inside a capture group.
ok (q{name="abc"} ~~ / 'name="' ( <-["]>+ ) '"' /) && ~$0 eq 'abc',
    'a `"` inside <-[...]> within a capture group parses (no spurious "Unmatched (")';

# 4. A forward-declared role used as a parameter type before its full
#    definition (the mutual-reference pattern).
my $r4 = EVAL 'role Fwd { ... }; role Use { method add(Fwd $f){ "added" } }; role Fwd does Use { method tag(){"fwd"} }; class K does Fwd {}; K.new.add(K.new)';
is $r4, 'added', 'forward-declared role works as a parameter type';

# 6. A multi method composed transitively via several roles is not a conflict.
lives-ok { EVAL 'role Routing { multi method add_route($r) { "added" } }; role Route does Routing {}; class Simple does Route {}; True' },
    'transitively-composed multi method is not a false X::Role::Composition::Conflict';

# 6b. ...even with a forward-declared stub role in the chain (Bailador shape).
lives-ok { EVAL 'role Rt { ... }; role Rting { multi method add_route(Rt $r) { "a" } }; role Rt does Rting {}; class Smpl does Rt {}; True' },
    'forward-stub + transitive multi method does not conflict';
