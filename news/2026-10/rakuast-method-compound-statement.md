# `$obj.attr OP= v` as a statement goes through the compound node

The statement parser had its own path for `$x.m += expr` and `$.n += expr`. It
built an `__mutsu_assign_method_lvalue` writeback by hand and wrapped it in the
compound marker. That path now builds the typed compound-assignment node, like
every other `OP=` (#14948), and lowers through the shared compound lowering:
`build_compound_assign_expr`'s method-call writeback.

The hand-built expansion is gone. The `.AST` text and the results match
rakudo 2026.09 and the previous build:
- `$d.s ~= "z"`;
- `$.n += 5`;
- `$e.a ,= 2`;
- `$f.n //= 7`.
