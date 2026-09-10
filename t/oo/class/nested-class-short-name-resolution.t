use Test;

plan 5;

# A class nested in a module registers fully qualified (`Foo::Params`); a sub
# in the same module must resolve the short bareword `Params` at runtime even
# when called from another package (JSON::Marshal's `MarshalParams.new` inside
# `marshal`, reached through META6's `to-json`).
module Foo {
    class Params { has $.x }
    my class Hidden { has $.y }
    sub mk() is export { Params.new(x => 1) }
    sub mk-hidden() is export { Hidden.new(y => 2) }
}
import Foo;
is mk().x, 1, 'module sub resolves a sibling nested class by short name';
is mk-hidden().y, 2, '... and a my-scoped nested class';

# Attribute.type on an attribute whose declared type is a nested class must
# report the resolved (qualified) type, as rakudo does — JSON::Unmarshal
# constructs nested typed attributes from it (META6's `has Support $.support`).
class OuterK {
    class InnerK { has $.v }
    has InnerK $.child is rw;
}
my $attr = OuterK.^attributes(:local).grep({ .name eq '$!child' })[0];
is $attr.type.^name, 'OuterK::InnerK', 'Attribute.type resolves a nested class type';

# The same via a custom attribute trait (the trait-time Attribute object path).
module TraitMod {
    multi sub trait_mod:<is> (Attribute $attr, :$tagged!) is export { }
}
import TraitMod;
class OuterT {
    class InnerT { has $.v }
    has InnerT $.child is rw is tagged;
}
my $attr2 = OuterT.^attributes(:local).grep({ .name eq '$!child' })[0];
is $attr2.type.^name, 'OuterT::InnerT', 'trait-time Attribute object resolves the type too';

# A global same-named class does not shadow the nested one for the owner.
class GShadow { has $.g }
class OuterS {
    class GShadow { has $.n }
    has GShadow $.child is rw;
}
my $attr3 = OuterS.^attributes(:local).grep({ .name eq '$!child' })[0];
is $attr3.type.^name, 'OuterS::GShadow', 'nested class shadows a same-named outer type';

done-testing;
