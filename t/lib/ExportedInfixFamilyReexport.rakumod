# Fixture for t/modules/import-export/exported-infix-family-ranks-candidates.t:
# re-exports the base module's whole export stash, as Red does with
# `Red::Operators::EXPORT::ALL::`.
use ExportedInfixFamilyBase;

sub EXPORT { ExportedInfixFamilyBase::EXPORT::ALL:: }
