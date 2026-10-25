# A declaration's subscript assignment is the typed node

`my @a.[2] = 42` is `(my @a).[2] = 42`. The declaration parser built its
assignment with the general target expansion (`assign_to_target_expr`), so a
`.AST` parse handed the converter an untyped `IndexAssign` as the tail of the
declaration. It now builds the typed assignment node through
`written_assignment`, like every other `=` (#14948). The `.AST` text and the
results are unchanged; they match rakudo 2026.09.

That was the last producer of a plain `IndexAssign` in a `.AST` parse. The three
`Expr::IndexAssign` arms of `rakuast::convert` that rendered `@a[i] = v`,
`%h<k> = v` and `%h{k} = v` are deleted. The arm for the indexed bind
(`@a[i] := v`) stays.

`t/rakuast/rakuast-decl-subscript-assign-node.t` pins the shapes and the
results.
