use Test;

# A public attribute named WHICH (`has ObjAt $.WHICH`) is an ordinary
# accessor method and overrides `Mu.WHICH`, including for Set membership;
# `self.Mu::WHICH` still reaches the object's own identity. Reduced from
# the Rake distribution, which computes a `ValueObjAt` into `$!WHICH` for
# value-type instances and falls back to `self.Mu::WHICH`.

plan 7;

class E { has $.WHICH = ValueObjAt.new("E|1") }
is E.new.WHICH, 'E|1', 'accessor wins over Mu.WHICH';
isa-ok E.new.WHICH, ValueObjAt, 'returns the attribute value';
my $m = 'WHICH';
is E.new."$m"(), 'E|1', 'dynamic method name too';
is (E.new, E.new).Set.elems, 1, 'Set membership uses the accessor';

class A { method own { self.Mu::WHICH } }
my $a = A.new;
is $a.own, $a.WHICH, 'Mu::WHICH is the object identity';
class B { has $.WHICH = "x"; method own { self.Mu::WHICH } }
ok B.new.own.Str.starts-with('B|'), 'Mu::WHICH bypasses an overriding accessor';
isnt B.new.own, B.new.own, 'distinct objects have distinct Mu::WHICH';
