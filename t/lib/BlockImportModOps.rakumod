unit module BlockImportModOps;

# Operators exported from an inner block, like FiniteFields' `FiniteField`
# module: a modular `-` that defers to the core candidate via `callsame`.
{
  multi infix:<->(UInt $a, UInt $b --> UInt) is export { callsame() mod $*modulus }
  multi prefix:<->(UInt $n --> UInt) is export { callsame() mod $*modulus }
}
