# Method objects answer the code rows; `Routine.yada` is a row

A method object from `.^lookup`, `.^find_method` or `.^methods` answers
`wrap`, `unwrap`, `assuming`, `cando` and `set_name` through the same
`Routine`/`Code` rows a `Sub` and a `&name` handle do (ADR-11276 §9.167), and
`Routine.yada` is a row: `&say.yada` answers `False`, as in Rakudo, where it was
no method.
