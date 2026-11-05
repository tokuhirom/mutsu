unit module TraitModIsProvider;

# Fixture for t/modules/trait-mod-is-imported-in-module.t (from Pop::Singleton):
# a role named like the trait argument, and the `trait_mod:<is>` that applies it.
role Singleton { my $single; method new { $single //= callsame } }

multi sub trait_mod:<is>(Mu:U $doee, :$Singleton!) is export {
    $doee.^add_role(Singleton);
}
