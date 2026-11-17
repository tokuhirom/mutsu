# Fixture for t/routines/dispatch/block-multi-operator-scope.t: a module that
# exports a candidate of a core infix for its own type.
unit module BlockOperatorImport;
class Box is export { }
multi infix:<+>(Box $a, Numeric() $b) is export { "imported" }
