# Fixture for t/modules/import-export/imported-operator-prec-record.t: an
# exported operator with a declared precedence, which an importer's own
# operators relate to.
unit module ImportedOpPrec;
sub infix:<±>($a, $b) is tighter(&infix:<*>) is assoc<right> is export { "($a±$b)" }
