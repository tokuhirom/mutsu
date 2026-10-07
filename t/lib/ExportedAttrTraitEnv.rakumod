# Shape of Trait::Env (ecosystem): two files each declare a `trait_mod:<is>`
# family, and the umbrella module re-exports the merged `&trait_mod:<is>`
# through `sub EXPORT`.
my %EXPORT;

module ExportedAttrTraitEnv {
    use ExportedAttrTraitEnvAttr;
    use ExportedAttrTraitEnvVar;

    %EXPORT<&trait_mod:<is>> = &trait_mod:<is>;
}

sub EXPORT { %EXPORT }
