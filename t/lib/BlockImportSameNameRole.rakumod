# Exports a role whose short name the importing program also declares as its
# own `my role` (ADR-8751).
unit module BlockImportSameNameRole;
role SameNameRole is export { method who { "module" } }
