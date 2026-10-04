unit module UnitModuleTrimConsts;
# Declares constants with the same names as PackagelessTrimConsts.
constant TRIM-BEFORE = "<";
constant TRIM-AFTER  = ">";
class Wrapper is export {
  method wrap(Str $s) { TRIM-BEFORE ~ $s ~ TRIM-AFTER }
}
