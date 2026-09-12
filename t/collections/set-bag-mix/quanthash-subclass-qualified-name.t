use Test;

# A QuantHash subclass declared inside a `module` is constructed by its
# package-qualified name, so `seed_quanthash_storage` asks the class registry
# for `M::Tally`'s declared parents on every `.new`. That lookup has a
# short-name fallback (a bare `Bar` finds a registered `Foo::Bar`) whose scan
# cannot match a name that is already qualified — skipping it for such a name
# must not change which base the subclass is backed by.
plan 9;

module M {
    role Tagged { method tag() { 'tagged' } }
    class Tally is BagHash does Tagged { }
    class Frozen is Bag does Tagged { }
    our sub build-tally() { Tally.new(<a b a>) }
    our sub build-frozen() { Frozen.new(<a b a>) }
}

my $t = M::build-tally();
is $t<a>, 2, 'a mutable QuantHash subclass counts its constructor arguments';
is $t.tag, 'tagged', 'the composed role is still there';
$t<a> = 5;
is $t<a>, 5, 'a BagHash subclass is backed by a MUTABLE bag';
is $t.elems, 2, 'and keeps its element count';
ok $t ~~ M::Tally, 'the instance is of the subclass';

my $f = M::build-frozen();
is $f<a>, 2, 'an immutable QuantHash subclass counts its arguments too';
is $f.tag, 'tagged', 'the composed role is there as well';
ok !(try { $f<a> = 5; True }), 'a Bag subclass keeps the immutable backing store';

# The short-name fallback itself: a bare parent name still resolves to the
# package-qualified registry entry it was declared under.
module N {
    class Base { method kind() { 'base' } }
    class Derived is Base { }
    our sub build() { Derived.new }
}
is N::build().kind, 'base', 'a bare parent name still resolves to its qualified class';
