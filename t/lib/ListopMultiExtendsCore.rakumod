unit module ListopMultiExtendsCore;

# ADR-0044 D1 fixture: exports a `multi sub splice` under one of the seven
# core listop names WITHOUT exporting its own `proto sub splice` -- unlike
# t/lib/ListopShadow.rakumod (which exports its own proto and so fully
# replaces the core dispatch set for that name), this candidate must ADD to
# CORE's existing splice dispatch set, leaving the core array form reachable.
multi sub splice(Str $s, Int $i, Str $r) is export { "custom $s $i $r" }
