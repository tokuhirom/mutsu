# Fixture for t/vm/nqp-core-type-operand-fold.t: a module exporting a type
# spelled like a CORE one, which shadows the CORE name in an importer.
unit module CoreNamedTypeExport;

class Pair is export { }
