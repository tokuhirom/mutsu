# `no strict`: a variable auto-declared in a block outlives the block

Under `no strict` an undeclared `$x` is the current package's `our` variable,
but mutsu kept it under a block-scoped env key, so
`no strict; { $a = 5 }; say $a` printed `(Any)` where rakudo prints `5`
(#10622). An array or hash autovivified by an element store
(`{ %h<x> = 1 }; say %h`) was lost the same way.

`no strict` is now recorded lexically in the env (`__mutsu_pragma::strict`, the
way `MONKEY-SEE-NO-EVAL` already was). Under that mark, a block's exit keeps
every variable the block used without declaring it, and stores it in the
package store. A read that misses every other store falls back to that package
variable. `use strict` and the default mode are unchanged.

This also retires the `OutsideBegin` guard from #10482. A nested `BEGIN` that
auto-declares a name under `no strict` used to stay unlifted whenever code
outside it mentioned the same name, because the lifted block's variable did not
outlive the block. It is now lifted like any other.
