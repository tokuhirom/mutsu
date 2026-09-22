# A `sub EXPORT` module (ADR-0087) whose exported operator subs are declared
# LOCALLY inside the hook's own body -- `Logic::Ternary`'s real shape, not the
# `UNIT::`-grep idiom `collect_unit_scope_routines` already approximates (that
# one only walks the module file's TOP-LEVEL statements, so a declaration
# nested inside `sub EXPORT`'s body is invisible to it) and not the
# `my &infix:<op> = sub {...}` shape `ExportHookValueTerm.rakumod` pins
# (which needs no parser help at all, since a custom infix WORD is accepted
# speculatively regardless of static knowledge). An actual `multi prefix:<...>
# (...) is export { ... }` declaration has no such fallback: `not3 5` parses
# as a plain listop call unless the parser already knows `not3` names a
# prefix operator, and a nested-in-EXPORT declaration like this one is not
# `UNIT::`-visible.
#
# Signature matches `Logic::Ternary`'s own EXPORT hook (`*@options --> Map()`)
# rather than a `(|)` capture -- deliberately, to stay a pin for the exact
# real-world shape this fix targets.
use v6.d;

sub EXPORT(*@options --> Map()) {
    multi prefix:<not3>(Int $x) is export { -$x }

    '&prefix:<not3>' => &prefix:<not3>,
}
