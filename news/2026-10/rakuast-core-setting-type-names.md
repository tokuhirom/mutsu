# RakuAST: barewords naming CORE setting types resolve like rakudo

`.AST` refused a bareword such as `X::AdHoc`, `IO::Path` or `Proc::Async`
unless it happened to be on a short hard-coded list of builtin type names.
Together the setting's qualified exception and I/O types were one of the most
frequent refusals under `MUTSU_RAKUAST=1`; every `throws-like { … }, X::…`
test hit one.

Rakudo resolves a bareword against the CORE setting at parse time. A name that
is a type object there renders as `Type::Simple` (measured on 2026.09). The
converter now has the same view of the setting: `src/rakuast/core_type_names.txt`,
1554 type names (500 of them `X::` exceptions) generated from rakudo by the new
`scripts/gen-core-type-names.raku`, which walks `CORE::` and its nested
stashes. The file is data taken from the reference implementation, not a
hand-kept list, and is regenerated rather than edited. Defined constants such
as `IterationEnd` are not types (raku renders them as `Term::Name`) and stay
out of it.

The round-trip ratchet grows from 1288 to 1580 of 5803 `t/` files. Pinned by
`t/rakuast/rakuast-core-type-names.t`, which passes under both mutsu
and raku, and a unit test of the lookup.
