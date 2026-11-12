# ADR-17557: A module whose BEGIN calls a loaded module's routine is run at parse time to learn its declarators

- **Status**: Accepted (route A of the maintainer's 2026-11-12 decision; route B is #17558)
- Date: 2026-11-12
- Deciders: tokuhirom, Claude
- Extends: [ADR-0124](0124-parse-time-export-probe-for-computed-export-stashes.md)
  (the parse-time export probe). ADR-0124 stays in force; this ADR adds one
  more module shape to its gate and one more thing the probe reads back.
- Related: [#17557](https://github.com/tokuhirom/mutsu/issues/17557) (this
  decision), [#17558](https://github.com/tokuhirom/mutsu/issues/17558) (the
  compile-time `use` that retires the probes), [#15371](https://github.com/tokuhirom/mutsu/issues/15371)
  (CompUnit::Util)

## 1. Context

rakudo compiles `use` at BEGIN time. A module's own BEGIN phasers run when
the module is compiled, and the importer sees what they produced. CompUnit::Util's
`re-exporthow` relies on this. It merges another unit's `EXPORTHOW` into the
loading module's own:

```raku
use CompUnit::Util :re-export;
need exports-stuff;                    # declares EXPORTHOW::DECLARE::pokemon
BEGIN re-exporthow('exports-stuff');
```

An importer of this module then writes `my pokemon pikachu { }`.

mutsu parses a whole unit before it runs any module. It learns a module's
declarators from a static scan of its `EXPORTHOW` packages, which cannot see
a merge done by a call at BEGIN time. CompUnit::Util's `t/02-re-export` and
`t/05-rexport-everything` died at parse time.

The maintainer weighed two routes:

- **A.** Widen ADR-0124's probe to this module shape.
- **B.** Load a `use`d module in the program's own interpreter when its `use`
  is parsed, as rakudo does.

The decision is A now and B next (#17558).

## 2. Decision

1. **Gate.** The module scan flags a module whose package spine has a BEGIN
   phaser that calls a routine which is not a core one, provided the module
   also loads another module (`use` / `need` of a name that is not a pragma).
   The routine can only come from such a module, and only such a routine can
   merge another unit's symbols at BEGIN. A call written as a statement
   (`Stmt::Call`, once the module's own `use` made the routine known) counts
   the same as a call expression. This is
   `ModuleScanResult::begin_calls_loaded_routine`, computed by
   `module_exports/decl_scan/begin_calls.rs`.
2. **Probe.** A flagged module goes through ADR-0124's probe. Besides the
   exported routines, the probe now reads back the `EXPORTHOW` declarators
   (`DECLARE` / `SUPERSEDE`) the load recorded for the module. The parser
   registers them as it registers scanned ones.
3. **Runtime half.** Two pieces record a BEGIN-time merge for the loading
   module (`ModuleState::exporthow_declares`):
   - A module's own BEGIN may bind its `EXPORTHOW` (`merge-generated-lexical-declaration`).
   - `merge-symbols` of a unit's `export-how-package` into it records that
     unit's declarators as the loading module's. A unit's `EXPORTHOW` stash
     carries the unit's name for this.
4. **Scope.** A declarator a `use` introduces now ends with the block of the
   `use`, as rakudo's lexical import does; a unit-level `use` keeps it for the
   unit. This applies to scanned declarators too, which previously leaked out
   of the block. Two imports of one declarator into the same scope still
   conflict (`X::EXPORTHOW::Conflict`), as on rakudo.
5. Everything else is as in ADR-0124: names are a superset, a failing probe
   contributes nothing, results are memoized per module and search path, and
   recursion is cut per lineage.

## 3. Consequences

- A flagged module's mainline runs twice: once in the probe and once for real.
  This is ADR-0124's trade-off, extended to one more shape. Its side effects
  outside the process (output, files) happen twice.
- The gate also flags a module whose BEGIN calls a loaded routine for any other
  reason. Such a module pays the second run without needing it. The gate is
  kept to the package spine and to modules that load a module to bound that cost.
- The scan cache stores only the flag, never the probed names, as in ADR-0124.

## 4. Alternatives

- **B, compile-time `use` (#17558).** This is the correct model: it runs each
  module once and needs no gate. It is a parser/loader re-architecture, chosen
  as the follow-up rather than the first step.
- **A gate on the routine names (`re-exporthow`, `re-export-everything`).**
  Rejected: that is a test-specific hack, and any module can merge symbols the
  same way.
