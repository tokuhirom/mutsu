# RakuAST preserves callback constraints and richer destructuring signatures

The RakuAST frontend retains callable parameter signatures, including callback
return types and the constraints used for binding and multi dispatch. A
whitespace callback signature uses `Parameter.sub-signature`; the colon form
keeps its constraint as model metadata because Rakudo omits it from `.raku`.
Single-parameter pointy blocks keep their full parameter definition whenever
a constraint requires it.

Anonymous slurpy destructuring, sigilless slurpy `where` constraints, named
array and nested Pair destructuring, and anonymous named sub-signatures now
cross the frontend boundary. Lowering reconstructs the existing binder's
parameter structure, including nested defaults and argument collection.
Pointy blocks also keep optional parameter defaults, caller container identity
and return constraints; parameters after `;;` retain their dispatch flags.
Sigilless pointy bindings also survive when a `whenever` statement takes its
block apart for execution.
The regression covers parsed trees, hand-built callback signatures, runtime
signature inspection, successful binding, rejected arguments and multi dispatch.
