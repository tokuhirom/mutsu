use Test;

plan 2;

# #8836: a public attribute's auto-generated accessor is not installed into
# `.^method_table` until the native `compose` step a custom `compose`
# override reaches via `callsame` (Rakudo's default `Metamodel::ClassHOW`
# implementation). A `compose` override that inspects `type.^method_table`
# BEFORE calling `callsame` -- exactly what AttrX::Lazy's
# `LazyAttributeContainerHOW.compose` does to check for an accessor name
# conflict -- must see the accessor still absent, matching raku. mutsu used
# to derive `.^method_table` unconditionally from `ClassDef::attributes`, so
# the accessor was always visible, even mid-composition.

my role ChecksMethodTableHOW {
    method compose(Mu \type) {
        state $seen-before;
        $seen-before = type.^method_table<checked-attr>:exists;
        type.^add_method('seen-before-compose', method (Mu:D:) { $seen-before });
        callsame;
    }
}
multi trait_mod:<is>(Attribute:D $attr, :$checks-method-table!) {
    my $class := $attr.package;
    unless $class.HOW ~~ ChecksMethodTableHOW {
        $class.HOW does ChecksMethodTableHOW;
    }
}
class Foo {
    has $.checked-attr is checks-method-table;
}

nok Foo.new.seen-before-compose,
    'a public attribute accessor is absent from .^method_table while composing';
ok Foo.^method_table<checked-attr>:exists,
    'the accessor appears in .^method_table once composition finishes';
