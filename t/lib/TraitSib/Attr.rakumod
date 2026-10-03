unit role TraitSib::Attr;

multi trait_mod:<is>(Attribute $attr, :$sib-attr!) is export {
    trait_mod:<is>($attr, :built);
    $attr does TraitSib::Attr;
}
