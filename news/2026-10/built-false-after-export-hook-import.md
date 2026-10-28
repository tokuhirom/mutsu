# `is built(False)` after importing a `sub EXPORT` module

A unit that imports a module computing its exports in `sub EXPORT` (for example `use paths`)
makes `False`/`True` shadowable terms. The RakuAST attribute lowering only accepted a plain Bool
literal as the argument of `is built(...)`, so the shadowable keyword turned the core trait into an
unknown custom trait ("Can't use unknown trait 'is' -> 'built'"). `MoarVM::Bytecode`, and with it
`Code::Coverage`, now load.
