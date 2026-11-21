# `let Type.method = v` and a bare `temp $obj.method;`

`temp Type.method = v` worked, but its neighbours did not (#18392):
`let A.table = "t"` died with `Confused. Two terms in a row`, because the
`let` statement only knew variables and subscripts, and a bare
`temp A.table;` (save now, assign later) died with `Unknown function: temp`.
`let $obj.x = 5` and a bare `temp $obj.x;` on an instance invocant were
broken the same way.

Every `temp`/`let` over a method-call lvalue — a variable, `self`, `$.attr`,
an element or a type-object invocant, with `=`, a compound assignment or
nothing after it — is now one `Stmt::TempMethodAssign`, parsed by a single
helper both statements share. A type object's save goes through the same
rw-accessor save/restore (`__mutsu_temp_method_save`) as an instance's,
instead of the separate `LEAVE`-phaser expansion the RakuAST lowering used
to build for it, so a `let` over one restores only when its scope fails
(`Nil`, an undefined value or an exception), exactly as in rakudo. The `.AST`
of all these forms matches rakudo's.
