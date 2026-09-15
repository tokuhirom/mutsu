use ReExportTraitProvider;

# JSON::Class re-exports JSON::Marshal's attribute traits exactly this way.
my package EXPORT::DEFAULT {
    OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;
    OUR::{'&re-export-greet'} := &re-export-greet;
}
