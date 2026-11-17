# RakuAST: a block's `multi` candidates join the outer family at CHECK time

A `multi` declared in a nested block under a name that an outer scope or the
setting already has a dispatcher for (`multi fam(Str)` in a block under a
top-level `multi fam(Int)`, a block-local `multi infix:<+>`) now resolves the
way rakudo's RakuAST frontend resolves it (#17826). A lookup of the name inside
the block answers a `RakuAST::VarDeclaration::Implicit::Constant` named `&fam`,
and its `compile-time-value` (and `.value`) is the proto rakudo derives for the
scope: the outer family's candidates followed by the block's own, so a
CHECK-time analysis such as Red's `red-compile` counts, inspects and calls the
whole family. Before, the resolution was a copy of the block's last candidate,
and its value was that one candidate.

The resolver records the outer resolution and the scope's candidates on the
node, because it has no interpreter; the family is built when the CHECK phase
asks for it. A nested block derives its family from the enclosing block's, so
it holds every enclosing scope's candidates. Over a setting operator, the value
is a dispatcher over the block's candidates that falls back to the operator,
as mutsu's run-time family is.

`value` of a RakuAST node is now served by an interpreter row. RakuAST nodes
share one dispatch shape, so a single `value` row answers for every node class,
and the derived family needs the interpreter to be built.
