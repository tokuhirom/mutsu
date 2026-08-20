# `.VAR.WHICH` now has one stable identity across a closure boundary

`{ my $v = 1; my $mk = -> { $v.VAR.WHICH }; say $mk() eq $v.VAR.WHICH }` used
to print `False` (raku: `True`), and so did the equivalent named-sub shape
(`sub f() { $v.VAR.WHICH }`) — a pre-existing bug independent of ADR-0032's
`WrapVarRef` container-capture generalization, which explicitly flagged this
case as needing its own design pass rather than a bolt-on fix.

## Root cause

`.VAR` builds a reflection `Instance` (class `Scalar`) representing a
variable's container and caches it per frame under a synthetic env key
(`__mutsu_var_meta::<name>`). The cache write is invisible to the compiler's
free-variable analysis (the key never appears in source text), so a `.VAR`
call made inside a closure cached its `Instance` into the closure's own env,
never propagating back to the declaring frame or to a sibling closure —
each independent `.VAR` call built its own `Instance` with a different
monotonic `id`, so `.WHICH` (keyed off `id`) never matched.

## Fix (ADR-0057)

Two changes compose to close the gap without inventing a new cross-frame
mechanism:

1. **Compile time**: `.VAR` on a free (captured) `$`-sigil variable now
   registers the same container-capture edge `WrapVarRef` sites do (reusing
   ADR-0032's D1/D2 machinery verbatim via a small factored-out helper,
   `Compiler::register_container_ref_capture_if_free`). This guarantees the
   variable is a shared `ContainerRef` cell by the time any closure/named-sub/
   method that reads it via `.VAR` actually runs — narrowly, only for names
   actually read through `.VAR` across a frame boundary.
2. **Runtime**: the `VAR` dispatch derives the reflection Instance's `id`
   from the shared cell's own stable heap address (`Gc::as_ptr`) instead of
   the process-global monotonic counter, whenever the target is currently
   boxed. Since `.WHICH` is purely `"{class_name}|{id}"`, two different
   `Instance` objects built independently in two different frames — as long
   as they resolve the SAME shared cell — compute the SAME id and therefore
   an identical `.WHICH`, with no cache write-back of any kind.

This deliberately avoids ADR-0032 §3's rejected "runtime name search of the
creating frame" alternative (a same-named shadow can be picked up by a
by-name guess at runtime) by doing the free-variable resolution at COMPILE
time, exactly the way `WrapVarRef` already does it safely.

## Verification

Both original repros now match `raku` (`True`). Additional shapes verified
against `raku`: three levels of nested closures all reading `.VAR.WHICH` on
the same outer variable agree; two independently captured variables never
collide. `t/closure-container-capture-alias.t` probe X (previously `todo`)
now passes unconditionally. The full set of ADR-0025/ADR-0032 pins this
change's blast radius could plausibly touch all stay green. Perf gate
(`roast/S32-num/int.t` on a release build) shows no trace of the ADR-0025
`#2749` broad-boxing regression — expected, since the new boxing trigger only
fires for a `.VAR` call on a free variable, which is rare by construction.

See `docs/adr/0057-var-reflection-identity-cell-address.md` for the full
design record.
