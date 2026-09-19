use Test;

plan 1;

# #8815: an attribute's OWN role mixin (`$attr does SomeRole` inside a custom
# `trait_mod:<is>`) must already be visible via `type.^attributes` by the time
# a role mixed into `$class.HOW` in the same handler has its `compose` hook
# run -- `apply_attribute_traits` used to store the attribute's mixed meta-
# object into the registry only AFTER calling `compose`, so a `compose`
# method that reads `type.^attributes.grep(SomeRole)` (the mechanism
# AttrX::Lazy's `LazyAttributeContainerHOW.compose` uses to find every
# `is lazy` attribute and install its accessor) always saw the pre-mixin
# object and found nothing.

my role MarkedAttr {
    has $.marked = True;
}
my role ComposeChecksAttrs {
    method compose(Mu \type) {
        my @marked = type.^attributes.grep(MarkedAttr);
        state $checked = 0;
        unless $checked {
            $checked = 1;
            type.^add_method("marked-count", method (Mu:D:) { @marked.elems });
        }
        callsame;
    }
}
multi trait_mod:<is>(Attribute:D $attr, :$marked!) {
    my $class := $attr.package;
    $attr does MarkedAttr;
    unless $class.HOW ~~ ComposeChecksAttrs {
        $class.HOW does ComposeChecksAttrs;
    }
}
class Foo {
    has $.x is marked;
}
is Foo.new.marked-count, 1,
    'a compose hook on $class.HOW sees the attribute\'s own role mixin via type.^attributes';
