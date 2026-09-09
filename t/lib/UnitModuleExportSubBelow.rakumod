# `sub EXPORT` declared BELOW the `unit module` line is inside the module's
# package, not the compunit's outer scope, so Raku never calls it as the
# module's custom export hook.
unit module UnitModuleExportSubBelow;

sub EXPORT(|) { Map.new('BelowAlias' => Int) }

our sub plain-export is export { 'ordinary export' }
