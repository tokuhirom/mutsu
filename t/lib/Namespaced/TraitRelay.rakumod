use ReExportTraitProvider;

# Keep the module name in the path rather than declaring `unit module`: this
# mirrors a number of ecosystem modules whose package-qualified declarations
# appear after their imports.
my package EXPORT::DEFAULT {
    OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;
}
