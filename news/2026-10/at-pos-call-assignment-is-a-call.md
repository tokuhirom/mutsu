# `$o.AT-POS($i) = $v` assigns through the call's result

The parser rewrote an assignment to an explicit `AT-POS` call into the element
store `$o[$i] = $v` (excepting a receiver spelled `self`). A user `ASSIGN-POS`
written `SELF.AT-POS($index) = $value` -- Red's `ColumnMethods` -- therefore
called itself until the stack ran out (Red t/37-json, `Too deep recursion`).
The rewrite is gone: the call goes through the ordinary method-lvalue path, and
a builtin receiver (`Array`, `Hash`, a `Seq`/`LazyList`) now assigns through the
container the call answers, as a type object already did.
