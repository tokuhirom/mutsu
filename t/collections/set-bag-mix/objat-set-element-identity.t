use Test;

# An ObjAt is identified by the identity it carries, not by the ObjAt object
# that holds it: `$o.WHICH.WHICH` is `ObjAt|<$o.WHICH>` on every call, so two
# ObjAts of one object are the same Set/Bag element. (From Tinky::JSON's
# t/020-construction.t, which compares state identities through sets.)

plan 10;

class P { }
my $p = P.new;

is $p.WHICH.WHICH, 'ObjAt|' ~ $p.WHICH, 'ObjAt.WHICH wraps the carried identity';
is $p.WHICH.WHICH, $p.WHICH.WHICH, 'and is stable across calls';
isa-ok $p.WHICH.WHICH, ObjAt, 'an ObjAt stays an ObjAt';
is "a".WHICH.WHICH, 'ValueObjAt|Str|a', 'a ValueObjAt keeps its class';
isa-ok "a".WHICH.WHICH, ValueObjAt, 'and is a ValueObjAt';

is set($p.WHICH, $p.WHICH).elems, 1, 'two ObjAts of one object are one Set element';
ok $p.WHICH.Set eqv $p.WHICH.Set, 'their Sets are eqv';
ok $p.WHICH ⊆ $p.WHICH, 'an ObjAt is a subset of an equal ObjAt';
nok $p.WHICH ⊆ P.new.WHICH, 'but not of a different object\'s';
is bag(1.WHICH, 1.WHICH){1.WHICH}, 2, 'ValueObjAts of one value are one Bag key';
