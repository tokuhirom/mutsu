# A precompiled module loads without reading its AST

When a module's compiled bytecode comes from the precompilation cache, its load
no longer reads the cached AST or walks it (ADR-12026 step 1). The
compiled-bytecode entry is now self-contained. Besides the compile, it stores:

- the parse effects;
- the module's *load facts*, the results of every pure AST pass
  `load_module_inner` runs: the recorded half of the undeclared-routine check,
  the unit package name, scope-name candidates, top-level `use`s, the
  unit-scope, `our` and dynamic name lists, exported operator and type names,
  and the state-sub and phaser flags;
- the AST its compile saw, kept encoded.

The AST is decoded only when something the facts cannot serve asks for it:
declarator docs, a `state` sub, a module-level block phaser, or a compile the
cache cannot reuse.

Two passes mixed AST and live state, so each is split into a recorded half and
a half re-asked on every load:

- the undeclared-routine check (the parser's import table and the registry);
- the module scope-name and premerge lists.

The cache key no longer hashes the whole compiled AST. It hashes only the
undeclared-routine guards, because the rest of the AST is the entry's own.
`MUTSU_PRECOMP_VERIFY=1` now also recomputes the facts from the entry's AST.

Measured on `use Test; ok 1;` minus an empty script (profiling build, warm
cache, callgrind): **43.6M → 35.1M instructions**.
