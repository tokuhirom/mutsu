# RakuAST keeps labelled blocks and deferred callable names

The RakuAST frontend now retains labels on do blocks and bare blocks without
turning them into loops. The parser keeps the bare-block spelling separately
from its ordinary do-block execution form. Label terms retain their original
Label value as hidden metadata, preserving identity and source location through
lowering. Label nodes expose the named `name` constructor field.
The `labels` accessor and attribute belong to the abstract Statement model;
local introspection excludes them from Statement::Expression, and inherited
attribute introspection retains their declaring package.
Constructed label terms resolve lexically while lowering their labelled
statement, with scope restored on every exit path. Parsed terms retain their
original Label values and source metadata.

Special callable method syntax uses `Call::BlockMethod`, including `.&?BLOCK`
and `.&*f`, with ordinary and hyper calls sharing the existing compiled dispatch.
The compiler's block and routine references have their own fieldless model
nodes, and constructed statement expressions accept labels and modifiers.

Callable binding declarations retain their `:=` intent in the shared binding
expansion, even when no runtime scalar/aggregate bookkeeping is needed. Bound
blocks, pointy blocks and existing routines expose `Initializer::Bind` and lower
through that same expansion; ordinary `=` remains an assignment initializer.

Names imported by a runtime EXPORT hook retain the parser's deferred choice
between a term and a zero-argument call. Hidden metadata records that choice on
the fallback call node; lowering restores the existing bytecode expression.
No import hook executes as part of this conversion.

Folded term keywords preserve the same deferred lookup and their fallback value.
Hook-installed terms keep value lookup semantics even when their names cannot be
predicted at parse time. A final bare hook term uses the same choice as one inside
a larger expression. Repeated string and AST EVALs seed only caller-visible
routine names, so a previously loaded module's private routine cannot override
a later hook-installed term.

The regression suite checks these forms together with lexical scope, recursive
callbacks, dynamic callables, tagged imports and hook-installed terms. The local
Rakudo oracle is 2026.07: it confirms the node classes and callable behavior,
but its Label.leave implementation is incomplete. The labelled leave regression
therefore preserves the behavior already pinned in `t/control/leave-statement.t`.
