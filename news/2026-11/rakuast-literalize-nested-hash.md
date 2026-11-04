# RakuAST: `literalize` turns a nested hash into a Map

`literalize` answered a `Map` only for the outermost hash of a constant: `{a => {b => 1}}` held an
inner `Hash`, and so did `(1, {a => 2})`. A hash anywhere in the value is now a `Map` and a
nested array a `List`, as on rakudo 2026.09 (#14916).
