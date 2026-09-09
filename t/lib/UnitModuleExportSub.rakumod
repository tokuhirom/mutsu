# `sub EXPORT` declared ABOVE the `unit module` line: it belongs to the
# compilation unit's outer scope, so `use` finds and runs it, and the Map it
# returns is the whole of what the importer gets.
sub EXPORT(|) {
    Map.new('MyAlias' => Int, '&exported-by-export' => sub { 'from EXPORT' })
}
unit module UnitModuleExportSub;

our sub not-exported { 'should not be visible' }
