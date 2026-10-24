# A `.=` assignment is the typed compound-assignment node

`$x .= meth(args)` moves onto `ParsedExpr::CompoundAssign` (#14948). Its `meta`
is `Dotty`, and its `source` is the method call written over the target.

`compound_lower::lower_dotty` is now the one lowering:
- As a statement, `$_ .= meth` is the topic form, which can write through a
  read-only whole-container topic (`given @a { .=reverse }`).
- Any other target goes through the `.=` expansion, which writes the result
  back through the target.

Every parser site builds the node:
- the leading `.=` statement and `EXPR .= meth` in `simple_expr_stmt`;
- `NAME .= meth` in `assign_stmt` and `try_assign`;
- the four postfix `.=` spellings (plain, quoted, dynamic and `&routine`).

`rakuast::lower` reads `ApplyDottyInfix(DottyInfix::CallAssign)` into the
same node and lowering.

Deleted:
- `parser::wrap_dot_assign`, whose callers now build the node;
- `rakuast::dynamic_method::retarget`, replaced by the one
  `retarget_dotty_call`;
- the duplicate topic/expression dispatch in `lower_dotty_assign`.

A hyper `.=` (`@a>>.=uc`) still renders an assignment over a hyper postfix
rather than rakudo's `ApplyDottyInfix` over `MetaPostfix::Hyper`. That
predates this change and is filed separately.

`t/rakuast/rakuast-dotty-assign-node.t` pins the shapes and the semantics.
