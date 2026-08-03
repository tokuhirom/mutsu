unit module ScopedProtoExport;

# A `proto`/`multi` pair that deliberately collides with the core `head`
# listop, so an importer can tell whether the import is still in scope.
proto sub head(|) is export {*}
multi sub head($what) { "module-head:$what" }

sub scoped-only() is export { "in scope" }
