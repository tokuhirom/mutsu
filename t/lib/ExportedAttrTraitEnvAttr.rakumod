unit module ExportedAttrTraitEnvAttr;

# Fixture shaped like ecosystem Trait::Env's Attribute.rakumod.
my role EnvStore {
    has $.env-name is rw;
    method scalar-build($, $default) { "{$!env-name}:{$default.raku}" }
}

multi sub trait_mod:<is>(Attribute $a, :%env) is export { apply-trait($a, 'hash') }
multi sub trait_mod:<is>(Attribute $a, List :$env) is export { apply-trait($a, 'list') }
multi sub trait_mod:<is>(Attribute $a, :$env) is export { apply-trait($a, 'scalar') }

sub apply-trait(Attribute $a, Str $kind) {
    $a does EnvStore;
    $a.env-name = "{$a.name.substr(2)}-$kind";
    $a.set_build( -> |c { $a.scalar-build(|c) } );
}
