unit module TraitMixinOuter;
need TraitMixinOuter::Raw;
our sub compress($data) is export { "outer:" ~ TraitMixinOuter::Raw::compress(1) }
