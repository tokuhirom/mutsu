# A `use` in a block binds its routines lexically

A `use` inside a block, a routine body or a closure body now binds the
routines it imports for the code that block lexically encloses, as rakudo
installs them in the block's pad (ADR-8751 stage 1, #8751). A closure built
in `sub mk { use M; -> { g() } }` keeps calling `M`'s `g` after `mk`
returned, even when the importing unit declares its own `sub g`; so does a
named sub declared in the block, a nested closure, and a closure created
*before* the `use` in the same block. Before, all of these called the unit's
own `g` (or died with "Unknown function"), and the Deps distribution's
`sub EXPORT { use Deps::Funcs; '&deps-scope' => -> |c { deps-scope |c, ... } }`
wrapper recursed into itself.

The import list names a lexical import set; the `use` fills it the first
time it runs, and every bareword call inside the block asks it first
(`OpCode::LexImportCall`). The stage-0 stopgap that probed the registry by
module name is gone. A callee written outside the block still sees the
import while the block runs; that waits for the registry aliases to retire.
