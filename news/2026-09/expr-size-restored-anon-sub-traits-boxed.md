# `Expr` is back to 120 bytes: anonymous-sub traits are boxed

`f775d825c` ("apply custom traits to anonymous subs") added an inline
`custom_traits: Vec<(String, Option<Expr>)>` to `Expr::AnonSubParams`. That
made it the widest variant, and every `Expr` grew from 120 to 128 bytes. The
parser and compiler recurse on `Expr` values, so each frame of that recursion
grew with it. In a debug build this was enough to overflow the 2 MiB stack of a
test thread, so `make test` aborted in
`runtime::run_dist::tests::eval_q_bracket_statement_list_runs_declaration_then_assertion`.
CI stayed green.

The traits now live in `AnonSubTraits`, one pointer to an optional boxed `Vec`.
Almost every anonymous sub has no custom traits, so there is usually no box at
all. `AnonSubTraits` derefs to a slice, so readers did not change. The new
`expr_size_guard` unit test pins `size_of::<Expr>()` at 120 bytes, the same way
`opcode_size_guard` already pins `OpCode`.
