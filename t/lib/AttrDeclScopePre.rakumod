unit module AttrDeclScopePre;
# Loads the types under an UNRELATED importer, so the role body's own `use`
# below takes the already-loaded path rather than the first-load one.
use AttrDeclScopeTypes;
