# ADR-0124: A module that binds its export stash under computed keys is run at parse time to learn the names

- Status: Proposed (implemented with #9500)
- Date: 2026-09-26
- Deciders: tokuhirom, Claude
- Extends: [ADR-0026](0026-slang-activation-architecture.md) §2.1 (compile-time
  `use` effect). ADR-0026 stays in force; this ADR widens the parse-time
  execution gate by one more, narrowly scanned, module shape.
- Related: [#9500](https://github.com/tokuhirom/mutsu/issues/9500) (this
  decision), [#9499](https://github.com/tokuhirom/mutsu/issues/9499) (the
  literal-key form, solved by the static scan),
  [ADR-0087](0087-runtime-export-hook-parse-time-approximation.md) (the import superset rule)

## 1. Context

Rakudo compiles `use` at BEGIN time: the module is loaded and its export stash
imported *before* the rest of the importing unit is parsed, so an operator the
module exports is part of the grammar from the next statement on. mutsu parses
the whole unit first and loads modules at run time; the parser learns a
module's exports from a static scan of its source
(`src/parser/stmt/simple/module_exports.rs`).

A static scan cannot see names a module computes. Moneys does:

```raku
my package EXPORT::ALL {
    for %currencies.keys -> $currency-code {
        OUR::{'&postfix:<' ~ $currency-code ~ '>'} := sub (Rat:D $amount) { ... };
    }
}
```

and its test writes `50.0CAD` and `(1.0USD).is-positive`. The run-time import
already worked (`&postfix:<USD>(5)` resolved); only the importer's *parse*
failed, because `USD` was not known to be a postfix operator.

ADR-0026 already runs one kind of module at parse time: a slang-activating
module, executed in a fresh interpreter on a fresh thread
(`runtime::slang_activation`).

## 2. Decision

1. **Gate.** The module scan sets `ModuleScanResult::dynamic_export_stash` when
   the module binds into one of its own export stashes (`EXPORT::<tag>`, the
   same naming rule the export collector uses) through `OUR::{KEY} := ...`
   with a `KEY` that is not a literal. The walk follows package nesting and
   the control flow a generated list is built with (`for`, `if`, `while`,
   `loop`, blocks). A literal key stays the static scan's job (#9499). No
   other module shape runs at parse time because of this ADR.
2. **Probe.** A `use` of a flagged module runs `use <module>` in a fresh
   `Interpreter` on a fresh registered user thread
   (`runtime::parse_time_exports::probe_module_exports`, the slang activation
   pattern), then reads back every routine the load recorded as exported —
   `exported_subs` plus the `&`-sigiled `exported_vars` — under any tag. The
   names are registered with the parser exactly like scanned exports.
3. **Superset, as ADR-0087.** Names of every tag are registered, not only
   those the `use`'s tag list admits: the parser set only answers "is this
   name a routine/operator", and run-time resolution still honours the tags.
4. **Failure is not a parse error.** A probe whose module dies while loading
   contributes nothing; the program's own run-time `use` loads the module
   again and reports the error at its real location.
5. **Caching.** The scan cache (in memory and on disk) stores only the flag,
   never the probed names — those depend on running code. Probe results are
   memoized per thread by `(module, search path)`, so a process runs each
   probe once.
6. **Recursion.** A probe's own parse may probe a further flagged module (its
   operators may appear in the probed module's source). A `use` cycle between
   flagged modules is cut by a per-lineage chain of modules being probed,
   inherited by each probe thread.

## 3. Consequences

- The module's mainline runs twice in a mutsu process that imports it — once
  in the probe, once for real — where rakudo runs it once. Side effects
  visible outside the process (output, files) happen twice. This is the same
  trade-off ADR-0026 accepted for slang activation, and the gate is narrow:
  only modules generating export names at run time pay it.
- The two interpreters do not share state; nothing the probe computes leaks
  into the program except the list of routine names.

## 4. Rejected

- **Evaluating the key expression statically** (constant-folding the loop over
  a `constant %hash`): works for Moneys, fails for any key drawn from a
  computation, file or environment, and grows into a second evaluator.
- **Running every imported module at parse time** (true BEGIN-time `use`):
  the general answer, but it changes load order, error timing and cost for
  every program. Out of scope; a future ADR can supersede this one.
- **Declaring the names by hand for known modules**: a test-specific hack.
