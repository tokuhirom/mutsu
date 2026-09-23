# Fixture for t/lang/core-type-fold-nqp-operand.t: a module exporting a type
# spelled like a CORE one, which shadows the CORE name in an importer.
unit module CoreNamedTypeExport;

class Pair is export { }
