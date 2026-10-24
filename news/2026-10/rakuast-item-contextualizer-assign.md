# `$(EXPR) = v` keeps its contextualizer in `.AST`

More of the assignment family moves onto the typed assignment node (#14948).

An assignment to an item contextualizer (`$($s) = 42`, `$(%h<k>) = 9`) now
builds `ParsedExpr::Assign` with the contextualizer as its target. `.AST`
renders the left side as `Contextualizer::Item`, as rakudo 2026.09 does,
instead of the variable inside it. The write-through to `EXPR` is the
lowering's (`rakuast::assign_lower`), and a hand-built tree lowers the same
way.

An assignment to an anonymous-state name or to a user's `$__c` is the typed
node too. The parser no longer keeps it in the old `Stmt::Assign` spelling.

The `:item` flag now follows rakudo's `$*LEFTSIGIL` in two more places. A
contextualizer sets the left sigil by its kind (`$(…)` item, `@(…)` and
`%(…)` list), and so does an anonymous `$`/`@`/`%`. `$ = 5` is now an item
assignment.

`t/rakuast/rakuast-item-contextualizer-assign.t` pins the shapes and the
semantics.
