# Fixture for t/modules/import-export/exported-operator-multi-vs-core-candidates.t (#17297).
unit module MemOpExport;
multi sub infix:<∈>(Mu $e, Mu $c) is export { "user" }
multi sub infix:<∋>(Mu $c, Mu $e) is export { "user" }
class Wide is export { has $.n }
multi sub infix:<∈>(Wide $e, Positional $c) is export { "narrow" }
