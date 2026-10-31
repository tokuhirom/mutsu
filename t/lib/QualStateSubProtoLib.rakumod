unit module QualStateSubProtoLib;

# Exports a `proto` whose candidates live in this module.
our proto rs($d, |) is export {*}
multi rs($data, *%args) { "multi" }
