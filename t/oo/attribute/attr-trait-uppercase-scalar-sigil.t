use Test;

plan 2;

# An uppercase-starting `is` trait on a `$`/`&`-sigil attribute must dispatch
# to a custom `trait_mod:<is>` handler exactly like a lowercase one, not be
# swallowed as the `@`/`%` "container type" trait (`has @.a is Array[Int]`,
# `has %.h is BagHash`). The parser used to treat ANY uppercase-starting `is`
# trait name as that container-type sugar regardless of sigil, so
# `has Str $.name is UTF8String;` (the pattern ASN::BER's ASN::Types module
# uses) silently discarded the trait instead of calling the module's own
# `multi trait_mod:<is>(Attribute $attr, :$UTF8String)` handler.

role Marker {
    method marked { True }
}

multi trait_mod:<is>(Attribute $attr, :$Marked) is export {
    $attr does Marker;
}

class Foo {
    has Str $.name is Marked;
}

ok Foo.^attributes.first(*.name eq '$!name') ~~ Marker,
    'uppercase custom trait on a $-sigil attribute reaches trait_mod:<is>';
ok Foo.^attributes.first(*.name eq '$!name').marked,
    'the composed role method is reachable through the attribute meta-object';

# vim: expandtab shiftwidth=4
