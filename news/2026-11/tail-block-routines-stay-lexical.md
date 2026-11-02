# Routines declared in a unit's last bare block stay lexical to it

A bare `{ ... }` as the last statement of a module (or script) was inlined
without its routine scope, so `multi infix:<->` declared in it stayed in the
registry for the rest of the program: the module's own subs saw the candidate
(`plain(5, 3)` returned the user operator's value where raku returns 2). Such a
block now runs through a real `BlockScope` like any other block that declares
routines. FiniteField's `{ multi infix:<*> ... }` tail block no longer made the
module's own `⁻¹` loop dispatch every `-`/`*` through user candidates: EC's
modular division dropped from ~1100 us to ~220-380 us and `t/secp256k1.t` from
92 s to 32 s (#15981).
