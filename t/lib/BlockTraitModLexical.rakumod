unit module BlockTraitModLexical;

# Exported `trait_mod:<is>` candidates for t/modules/import-export/
# use-in-block-binds-trait-mod-lexically.t (#8751).

multi sub trait_mod:<is>(Routine:D $r, :$btml-tagged!) is export {
    $r.wrap(-> |c { 'tagged ' ~ callsame });
}
multi sub trait_mod:<is>(Variable:D $v, :$btml-tagged!) is export {
    $v.var = 'tagged var';
}
multi sub trait_mod:<is>(Attribute:D $a, :$btml-tagged!) is export {
    $a does role { method btml-tag { 'tagged attr' } };
}
