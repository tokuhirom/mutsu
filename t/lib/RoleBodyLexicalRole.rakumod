unit module RoleBodyLexicalRole;

# Shape of the ValueType distribution: a `my role` lexical to the module is
# named by a role body that a class in ANOTHER file composes.
my role Excluded {}
multi trait_mod:<is>(Attribute:D $attr, :$hidden-here!) is export {
    $attr does Excluded
}

role Counted is export {
    my @attrs = ::?CLASS.^attributes.map: { $_ unless $_ ~~ Excluded };
    method counted-names { @attrs.map(*.name).List }
}
