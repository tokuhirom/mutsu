# `match`, `subst`, `trans`, `indices`, `IO`, `sprintf` and `printf` are method-table rows

The text methods that need the interpreter are now method-table rows (ADR-11276 §9.75): `match`
on `Str`; `subst`, `trans`, `indices` and `IO` on `Str` and `Cool`; `sprintf` and `printf` on
`Cool`. Each row calls the one interpreter routine the by-name dispatch arm also calls, and
`subst` still tries the VM's native substitution first. Together with §9.74 this leaves only
`split` and `fmt` in the `text` group of the migration inventory.
