# Exports the names a program imports at its top level, which the routines of
# another loaded module (ModuleRoutineScopeProbe) must not see.
unit module ModuleRoutineScopeExporter;

class ImportedType is export { method hi { 'imported' } }
our constant IMPORTED-K is export = 42;
