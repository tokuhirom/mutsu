# Fixture for t/modules/import-export/export-sub-returns-stash.t: re-exports
# another module's whole export stash, as Test::Describe does with `Test::EXPORT::ALL::`.
use ExportStashBase;

sub EXPORT { ExportStashBase::EXPORT::ALL:: }
