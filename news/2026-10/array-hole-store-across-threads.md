# Array gaps grown across threads are real holes

`@a[$i] = $v` past the end of an array, made inside a `start` block (or once
any thread had run), went through the cross-thread lane's own copy of the
element store, which grew the array with `Nil`s and dropped its record of
which slots were assigned. `my @a; await start { @a[1] = 1 }; say @a` printed
`[Nil 1]`, and `@a[0]:exists` answered True; Rakudo prints `[(Any) 1]` and
False.

Both lane copies now call `ArrayData::store_element`, the single `@a[$i] = $v`
store the `[]=` opcode and `ASSIGN-POS` already share (ADR-0118), so gaps are
`Any` holes (or the `is default` value) and only assigned slots exist.
