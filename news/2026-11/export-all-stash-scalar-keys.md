# `EXPORT::ALL.WHO` keys an exported scalar as `$name`

Slice of #16819. A `$`-scalar exported with `is export` was published in the module's `EXPORT::<tag>`
stash as `name`; rakudo keys it `$name` (arrays, hashes and subs already carried their sigils, terms,
types and enum keys stay bare). The compiler now tells the runtime which exported variables are
`$`-scalars (`RegisterVarExport.scalar` -> `ModuleState::exported_scalar_vars`) and the stash build
adds the sigil.
