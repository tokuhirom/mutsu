# A closure keeps the routines its block's `use` imported

A closure built inside a block that `use`s a module (typically `sub EXPORT { use Inner; -> |c { greet(|c) } }`)
lost the imported routines once the block's dynamic import scope popped: the call died with `Unknown function`,
and when the closure was exported under the imported routine's own name it resolved to itself and recursed until
the stack overflowed (the Deps distribution). Rakudo installs a `use`'s symbols in the enclosing lexical scope, so
the closure keeps them and they win over a same-named routine of the importing unit.

The compiler now records the modules a non-top-level `use` names on the `CompiledCode` of the enclosing block and
of every sub or closure nested in it, and a bareword call there resolves the module's exported routine before the
importer's `&name`. This is a stopgap marked `TODO` in `vm_lexical_use.rs`: the lasting fix is to bind the import
lexically in the block's frame, which is the imports step of ADR-12529 phase 2. Calls outside the block still die,
as `roast/S11-modules/lexical.t` requires.

Refs #8751.
