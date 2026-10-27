# Fixture for t/modules/import-export/exported-infix-family-ranks-candidates.t:
# `infix:<==>` candidates whose coercion parameters (`DateTime()`, `Date()`) both
# accept a DateTime operand, declared in the order a stash re-export need not keep.
unit module ExportedInfixFamilyBase;

class Node is export { has $.tag }

multi infix:<==>(Node $a, Date() $b is rw) is export { "Date()" }
multi infix:<==>(Node $a, Date() $b is readonly) is export { "Date() ro" }
multi infix:<==>(Node $a, DateTime() $b is rw) is export { "DateTime()" }
multi infix:<==>(Node $a, DateTime() $b is readonly) is export { "DateTime() ro" }
