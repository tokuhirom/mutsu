use v6;
use Test;

# A module that computes its exports through a run-time `sub EXPORT` hook has
# no `is export` traits for the static scan to find (ADR-0087). The parser
# approximates the export set from the module's own UNIT-SCOPE routines, but
# that walk never descends into `sub EXPORT`'s own body -- so a distribution
# that declares its operator subs LOCALLY inside the hook, to close over the
# `use` arguments, was invisible to it. `t/lib/ExportHookLocalPrefixOp.rakumod`
# is that shape (`Logic::Ternary`'s real one): a `multi prefix:<not3>(...) is
# export { ... }` declared inside `sub EXPORT`.
#
# Without registering it, `not3 5` parsed as an ordinary listop call to an
# undeclared function and died at run time with "Unknown function: not3" --
# every one of Logic::Ternary's five roast files hit this (0/5 baseline files
# at parity).

plan 2;

use lib 't/lib';
use ExportHookLocalPrefixOp;

is (not3 5), -5, 'a prefix op declared LOCALLY inside the EXPORT hook parses as a named prefix operator';
is (not3 -5), 5, 'same, positive result';
