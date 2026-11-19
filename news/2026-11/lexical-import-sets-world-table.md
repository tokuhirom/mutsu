# A routine imported after a worker started is callable on that worker

ADR-8751 binds a module's imported routines through lexical import sets,
which each `use` fills the first time it runs. The sets lived in the
per-thread module state, which a spawn forks. So a worker started before a
`use` ran never saw what that `use` imported. That covers a worker started at
`BEGIN`, or by a module's mainline during `use`. A closure calling the
imported routine there died with `Unknown function` (#18137). The block-scoped
form, `{ use Test; $channel.send({ plan 1; ... }) }`, failed the same way.

The sets, together with their multi families, hook values, operator sets and
each unit's top-level sets, are now one world table shared by every thread
(ADR-17216). The `LexImportCall` site memos are keyed by its world generation.
