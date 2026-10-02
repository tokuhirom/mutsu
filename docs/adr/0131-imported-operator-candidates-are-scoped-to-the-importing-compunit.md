# ADR-0131: An imported operator candidate is visible only to its declaring and importing compunits

- Status: Accepted (implemented)
- Date: 2026-09-27
- Resolves: [#9944](https://github.com/tokuhirom/mutsu/issues/9944)
- Related: [ADR-0081](0081-compunit-scoped-module-import-aliases.md) (compunit
  ownership for imported variables and types),
  [ADR-0108](0108-closure-must-pin-its-defining-blocks-routine-imports.md)
  (the same "flat registry, no lexical import scope" gap for closures),
  [ADR-0006](0006-baseline-interpreter-optimizations.md) §2.1 (constant folding
  against user operators)

## Context

Raku resolves an operator lexically. Inside module `B`, `$a * $b` sees the core
`infix:<*>` plus the candidates `B` declared or imported. It never sees a
candidate that another compunit imported.

mutsu's routine registry is flat. A script's operator imports, and a `unit
module` body's, are installed as `GLOBAL::infix:<*>/…` aliases. A bare-name
operator walk (`resolve_function_with_types`) from any module reaches
`GLOBAL`, so it reached those aliases. The name-level gate
`user_declared_infix_ops` did not stop this. An export recorded an *empty*
unit set, and an empty set meant "visible everywhere".

In the Bitcoin distribution this made every integer `*` in `FiniteField` run
`secp256k1`'s `where 1 < $n < 2**256` clause. One point operation took 0.8 s,
and the test suite timed out after 15 minutes (rakudo: 12 s).

## Decision

An operator import binds the operator to the **importing compilation unit**.
Visibility is checked at two layers, both keyed by compunit:

1. **Name-level gate.** `import_module` and `install_export_symbol` add the
   importing unit (`current_unit`) to `user_declared_infix_ops[op]`. They no
   longer clear the set to "visible everywhere". A unit that neither declared
   nor imported `infix:<op>` keeps the native fast paths. It also never enters
   the candidate walk.
2. **Candidate level.** `operator_import_units[op][declaring unit]` records
   every unit that imported that candidate family. The declaring unit is the
   candidate's `source_file`. `choose_best_matching_candidate_excluding` and
   the plain-routine and arity-fallback returns of `resolve_function_with_types`
   drop a recorded candidate unless the running code's unit chain contains its
   declaring unit or one of its importers. A unit that has an `infix:<*>` of
   its own therefore still ignores a family that only another unit imported. A
   candidate that was never imported has no record and is not filtered.

The running unit has two anchors, `current_unit` and the executing frame's
`def_file`, the same as `prelude_visible_here`. Not every method-dispatch path
switches `current_unit`. Checking the frame's file keeps a module's own methods
able to see what that module imported, whoever calls them. Both anchors walk
the `EVAL` parent chain.

Three supporting changes make the importing unit and scope correct:

- A role's deferred body re-run (`enter_source_file`) now switches
  `current_unit` to the role's file together with `?FILE`. A `use` in the body
  then imports into the role's compunit, not into the compunit that composes
  the role.
- A `use` inside a package body (`class K { use Op; … }`) imports its operators
  into that package, not the enclosing `unit module`. That module's other
  routines then do not see them. The unit-package target is used only while
  the runtime package is still `GLOBAL`, which is the pre-`unit` declaration
  window it was added for.
- Method bodies and declaration chunks (class bodies) share the unit's
  `FoldCtx`. Before this, a literal `2 ** 3` in a method was folded against the
  core operator, even though the unit `use`d a module (or had a class-body
  `use`) that may export one. The unit-level refold pass now covers them.

## Consequences

- An imported `where`-guarded candidate costs nothing in modules that did not
  import it.
- `plain_fn_resolve_memo` is keyed by `current_unit` alone, so it is bypassed
  for an operator name that has an import record. Only the operator's own
  resolutions pay for this.
- Visibility is per compunit, not per block. A block-scoped `use` inside a
  routine still sets the unit's name-level gate for the rest of the unit. The
  registry alias it installed is still removed by `pop_import_scope`, so the
  result is correct. The unit only takes the slower dispatch path.
- A candidate that is declared but never exported keeps its registry key. Its
  name-level gate already confines it to its declaring unit, so only a unit
  with a same-named operator in scope reaches it. Scoping those candidates by
  declaring unit needs a reliable `source_file` ↔ unit mapping for `EVAL`
  units, and is left for later.

## Alternatives rejected

- **Re-key operator aliases per importing compunit in the registry.** Every
  registry reader would have to learn the new key shape. Multi-candidate
  merging (`import_multi_candidate_merged`) already shares one key between
  importers.
- **Compile-time binding of the operator's candidate family.** mutsu loads
  modules at runtime, after the consuming unit is compiled, so the family is
  not known at compile time.

## Amendment (2026-10-02): package-less multi/proto families of a loaded module (#11004)

**Context.** A module with no package of its own (a bare file, or the top level
of a file whose `unit module` mutsu still registers under `GLOBAL`) registers
its `multi` candidates and `proto` under the shared `GLOBAL::name/<sig>` and
`GLOBAL::name` keys. Raku makes them lexicals of that compunit. `is export` only
puts them in the `EXPORT` stash, and only an import makes them visible to
another compunit. The per-name seclusion of private `sub`s
(`unit_private_routines.rs`, #7558) cannot move them, because it holds one
routine per name and multi candidates are additive across compunits. So
`need`, `CompUnit::Repository.need` and an unexported multi family all leaked
into the loading scope.

**Decision.** The candidate-level gate of this ADR now covers those families,
not only imported operators:

- After a module load (`load_module_inner`, `require`), each package-less
  family the module itself declared (a candidate or proto whose `source_file`
  is the module) gets an `operator_import_units[name][module unit]` record with
  **no importers**. `MAIN`, prelude splices and `our` routines are excluded.
  They keep their own handling.
- `import_module` adds the importing unit to a family that already has a
  record. An operator family still gets a record when it is first imported.
- The candidate walks that already filter with this gate need no change. The
  name probes (`has_proto`, `has_multi_candidates`, `has_multi_function`), the
  bare proto lookup and the `&name` candidate gather filter on it too. A family
  that only another compunit can see is then *undeclared* here
  (`X::Undeclared::Symbols`), not declared but uncallable. Their caches are
  keyed without the unit, so they are bypassed for a scoped name.

Two supporting changes keep a scoped family callable where it should be:

- A candidate is matched in its declaring compunit as well as its package
  (`args_match_multi_candidate_in_scope`, `with_candidate_scope`). Its `where`
  clauses and defaults are that unit's code, so they see the families that unit
  imported, whoever is calling.
- A `&name` code value of a multi carries its captured candidates. When the
  calling unit cannot see the family by name (a module invoking a `&sha256` its
  importer passed in), the value dispatches it from the candidates' declaring
  unit. Before, it fell back to a first-that-binds trial over the captured
  list, without ranking.

**Consequences.** A module's unexported family is no longer callable from
outside the module, and a family a module imported is not visible to that
module's own importer. Both match Rakudo. Candidates the *main script*
declares are still unscoped. A module whose family shares a name with them
still sees the script's candidates (#11081).
