# Fixture for t/modules/import-export/use-reads-loaded-unit.t: a module whose
# own source uses an operator another module computes the name of while it
# loads, so the module's own compile has to load that module first.
unit module UsesComputedStashInside;
use ComputedStashLoud;
say "UsesComputedStashInside loaded";
sub inside-zz is export { 9ZZ }
