# RakuAST: `does R[]` round-trips

An empty role application (`class C does R[] { }`) used to render
`Type::Parameterized` with an empty `args => ArgList.new()` and then refused to
lower. Rakudo omits `args` entirely in that case and its accessor answers an
empty `ArgList`; the converter now does the same, the lowerer accepts the
missing field as `R[]`, and `Type::Parameterized` declares `args` as an
optional field. `roast/S14-roles/parameterized-basic.t` passes under
`MUTSU_RAKUAST=1`.
