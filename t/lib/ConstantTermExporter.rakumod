unit module ConstantTermExporter;

# Fixture for t/vm/binding/constant-term-vs-same-named-scalar.t: an exported
# sigil-less constant whose spelling the importer also uses for a `$`-scalar.
constant ctx-term is export = 'term';
