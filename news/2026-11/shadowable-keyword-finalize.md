# Shadowable term keywords are carried through finalization

ADR-15760 slice 2: in a unit that imported through a run-time `sub EXPORT` hook, `True`, `False`,
`Nil`, `Empty` and `Any` are the parser's `ShadowableTermKeyword` (an `EXPORT`-installed binding of
the same name wins at run time, #9047). The lowering gives that expression back unchanged, so
`finalize_unit` keeps it as the parser built it instead of converting and lowering it back.
