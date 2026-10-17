unit module UseInBlockTypesConsts;

# Exported types and constants, for
# t/modules/import-export/use-in-block-hides-types-and-constants-from-callees.t
class UibFoo is export { method hi { 'imported' } }
constant UIB-KONST is export = 42;
constant uib-lower is export = 'lower';
