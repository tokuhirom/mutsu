# Exports a class whose short name the importing program also declares as its
# own `my class` (ADR-8751): a block-scoped `use` must not take that name away
# from the program's routines.
unit module BlockImportSameNameType;
class SameNameCls is export { method hi { "module" } }
