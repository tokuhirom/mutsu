class MetamodelX::EarlyHOW is Metamodel::ClassHOW {
    has Bool $!saw-attribute-trait;

    method note-attribute-trait(Mu:U \type, Attribute $attribute) {
        $!saw-attribute-trait = True;
    }

    method saw-attribute-trait() { $!saw-attribute-trait }
}

multi trait_mod:<is>(Attribute $attribute, :&early-trait! --> Empty) is export {
    $attribute.package.^note-attribute-trait: $attribute;
}

my package EXPORTHOW {
    package DECLARE {
        constant early = MetamodelX::EarlyHOW;
    }
}
