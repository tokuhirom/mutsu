# An assignment statement to a variable is the typed assignment node

The second slice of the assignment family (#14948). The statement parser's
plain `=` to a variable (`$x = …`, `@a = …`, `%h = …`, `&f = …`) now builds
the same `ParsedExpr::Assign` node that an assignment in expression position
builds.

- An executing parse lowers the node through
  `rakuast::assign_lower::lower_statement`, which owns the `Stmt::Assign`
  form.
- A `.AST` parse keeps the node. A loose word-logical tail stays part of it:
  `$x = 1 and say 2` is the one `ApplyInfix(assignment, and, …)` that rakudo
  has, not an assignment followed by a re-read of `$x`.

Three shapes now match rakudo 2026.09:

- A code variable target renders as `Var::Lexical("&f")` with a list
  assignment. Before, it was `$&f` with `:item`.
- A placeholder target (`$^x = 9`, `@_ = 7`) renders as the placeholder
  declaration the block makes there.
- An assignment to a scalar placeholder is an item assignment: placeholders
  set the expression's left sigil the way variables do.

A hand-built `ApplyInfix` assigning to a placeholder now lowers as well.

A double-underscore name (an anonymous variable's minted name, or a user's
`$__c`) still takes the old `Stmt::Assign` spelling. The conversion's arm
for it moves in a later slice.

`t/rakuast/rakuast-statement-assignment-node.t` pins the shapes and the
semantics.
