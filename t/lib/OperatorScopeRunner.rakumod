use MONKEY-SEE-NO-EVAL;
unit module OperatorScopeRunner;

# A module routine that invokes a block handed to it by its caller. The block
# was compiled in the CALLER's compilation unit, so the caller's operators are
# what must apply inside it -- not this module's.
sub run-block(&code) is export { code() }

# Operators used by the MODULE itself. A caller's `sub infix:<+>` /
# `sub infix:<~>` is lexically scoped to the caller's unit and must never
# reach either of these.
sub module-sum($a, $b) is export { $a + $b }
sub module-concat($a, $b) is export { $a ~ $b }

# The module EVALs a string of its own. Operators from the calling unit are
# not in scope here either.
sub module-eval(Str $code) is export { EVAL $code }
