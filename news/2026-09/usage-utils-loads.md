# Usage::Utils loads: definite-return scope, grammar exports, imported parents

`Usage::Utils` (ecosystem, `blocked_load`) now loads and passes its test files. Three general
fixes were needed.

**The definite-return check covers only the routine's own scope.** A routine whose signature pins
its return value (`--> True`, `--> Nil`, `--> 42`) may not `return` an argument. mutsu rejected
every such `return` anywhere in the body. Rakudo resets the signature info in every nested
`block`/`pblock`, so a `return True` inside `if $x { ... }` is accepted and returns its own
argument; only a `return` in the routine's own scope is rejected at compile time, including the
statement-modifier forms (`return 1 if $x`), which open no block. At run time only the `.return`
method checks the pinned value (rakudo's `Mu.return` → `check-signature`); the `return` routine
never does. The two copies of the compile-time walker are now one function, and the return signal
carries whether it came from `.return`.

**`grammar G is export` exports the grammar.** The grammar declaration parser skipped the `is
export` trait without recording the export, so `::('G')` failed in the importer and the grammar was
reachable only through fallbacks. It now emits the same export record a `class` does.

**An imported type is a real parent inside a `unit module`.** The compiler pre-qualifies a bare
parent to `Module::Name`, which is right for a sibling but not for an imported type. Validation
already resolved `Foo::C` to the imported `P::C`, but the class kept the phantom name, so its MRO
ended there: `grammar UsageStr is BasePaths` lost `Grammar` and every inherited token. The resolved
name is now what the class stores.

Pinned by `t/routines/call/return-value-spec-nested-block.t` and
`t/modules/import-export/unit-module-imported-parent.t`.
