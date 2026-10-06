use Test;

# From the `immutable` distribution (via ValueMap, which subclasses Map and
# captures `Map.^lookup('new')` / `Map.^lookup('raku')` in constants).

plan 9;

my constant &map-new  = Map.^lookup('new');
my constant &map-keys = Map.^lookup('keys');

is map-new(Map, (:a(1),)).keys.join, 'a', 'core Method object .new called on the type object';

class MyMap is Map {
    method new(|c) { map-new(self, |c) }
}
my $vm = MyMap.new((:a(1), :b(2)));
isa-ok $vm, MyMap, 'a Method-object constructor called from a subclass new does not recurse';
is $vm.elems, 2, 'and the instance holds the elements';

is-deeply map-keys($vm).sort.list, ('a', 'b'), 'core Method object called on a subclass instance';

class Plain is Map { }
is Plain.new((:x(1), :y(2))).elems, 2, 'class is Map takes a positional list in new';
isa-ok Plain.new((:x(1),)), Plain, 'and builds the subclass';

class Via is Map {
    method new(|c) { self.Map::new(|c) }
}
is Via.new((:q(5),)).elems, 1, 'self.Map::new from a subclass new reaches the core constructor';

class Over is Map {
    multi method keys(Over:D:) { <over> }
}
is Over.new((:a(1),)).keys.join, 'over', 'subclass override still wins for a normal call';
is-deeply map-keys(Over.new((:a(1),))).list, ('a',), 'but the Method object runs the core candidate';
