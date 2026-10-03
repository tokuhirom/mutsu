# ADR-11136: A module's GLOBAL merge is lexical to the scope that loaded it

- **Status**: Accepted (user decision 2026-10-03). Implementation: see §6.
- **Date**: 2026-10-03
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11136](https://github.com/tokuhirom/mutsu/issues/11136) (split out of
  [#11103](https://github.com/tokuhirom/mutsu/issues/11103))
- **Related**: [ADR-0081](0081-compunit-scoped-module-import-aliases.md) (a unit module's
  *imported aliases* are scoped to its compunit), [ADR-0092](0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md)
  (closure capture is a chained env tier), [ADR-0108](0108-closure-must-pin-its-defining-blocks-routine-imports.md)
  (a closure must keep its defining block's imports), and #7797 (`compunit_visible_packages`,
  the unit-level grant this ADR extends to bare names and to block granularity).

## 1. Context

### 1.1 What Rakudo does

A module's own package-scope declarations (its `class`es, `role`s, `package`s, `our sub`s,
`constant`s) live in the module compunit's `GLOBALish`. `need`/`use`/`require` merges that
stash into the **lexical scope that holds the statement**, not into the process's `GLOBAL`.
Measured on Rakudo 2026.09 with three fixture modules: `Outer` (no `unit` declarator) does
`use Inner;` and declares `class OuterCls`, `our sub outer-our`, `constant OUTER-C`,
`package OuterPkg { our sub f }` and `class Outer::Nested`. `Unit` says `unit module Unit;` and
declares `class UnitCls`.

| probe (an `EVAL` placed lexically at the probe point) | inside `{ need Outer; need Unit; ... }` | after that block |
| --- | --- | --- |
| `OuterCls.v`, `outer-our()`, `OUTER-C` | resolve | **undeclared** |
| `OuterPkg::f()`, `Outer::Nested.v`, `Unit::UnitCls.v` | resolve | **undeclared** |
| `::('OuterCls')` | resolves | `Failure` |
| `InnerCls.v`, `inner-our()` (merged into `Outer`, not here) | **undeclared** | undeclared |
| bare `UnitCls` (it is `Unit::UnitCls`) | undeclared | undeclared |
| `GLOBAL::OuterCls`, `GLOBAL::<OuterCls>:exists` | not found / `False` | not found / `False` |

The rest of the table:

- An `OuterCls` instance made inside the block keeps working after it: `.v`, `.^name`, and its
  methods that name `InnerCls`/`OuterCls` themselves.
- A closure created inside the block and called after it still resolves `OuterCls`.
- A `need` in a sub body is gone once the sub returns.
- A second block that `need`s `Outer` sees it again.
- An `EVAL` inside a sub declared *outside* the `need` block does not see the names, even while
  the block is running.

So the merge has three properties:

- **lexical**: it follows the code location, not the call stack;
- **block-scoped**: it ends with the scope that holds the statement;
- **not transitive**: an importer sees what the module *declares*, never what the module itself
  `use`d.

### 1.2 What mutsu does

mutsu's type, package and `our` stores are process-global and keyed by name. A module load
publishes its declarations into them and keeps them for the life of the process, on purpose:
`loaded_modules` never rolls back, so a later re-`use` is a no-op that must still find them
(`persistent_classes`, `module_registered_functions`, `package_globals`).

Visibility is a separate, partial layer on top of those stores:

- **#7797**: a *qualified* name `Pkg::x` is visible only to a compunit that loaded `Pkg` directly
  (`compunit_visible_packages`, consulted by `qualified_name_visible_here`). It is unit-level
  only, so a block-scoped `need` grants the whole file forever.
- **#11144**: a package-less module's `my`-scoped `sub f is export` is private to the module.
- **Nothing** gates a *bare* name. `OuterCls`, `outer-our()` and `OUTER-C` resolve everywhere
  after any load, including from code that never loaded `Outer` and through a module that `use`d
  it (`InnerCls` in the table above).

## 2. Decision

Keep the stores global. Visibility is decided by **provenance + merges**:

1. **Provenance.** When a module load finishes, every bare name its own body published
   (classes, roles, enums, subsets, packages, `our` subs, `our`/`constant` terms) is attributed
   to that module. Names a nested load already attributed keep their attribution. A name that
   existed before the load is never attributed: the program's own declarations stay
   unconditionally visible.
2. **Merges.** A `need`/`use`/`require` merges the module into the scope that executes it:
   - **block level** when an import scope (`OpCode::ImportScope`) opened by the *same compunit*
     is innermost. The merge is a scope-recorded env key (`MetaNs`), so it disappears with the
     block's env tier like an imported alias does. A closure created inside the block captures
     the tier (ADR-0092), so it keeps the merge, as in Rakudo.
   - **unit level** otherwise (the statement is at the compunit's top level). The merge is
     recorded per compunit, as #7797's grants already are. An `EVAL` is its own compunit with a
     unique unit symbol, so its top-level merges end with it.

   A module's own mainline is a compunit of its own. Its `use Inner` merges `Inner` into
   `Outer`'s unit, never into the importer's scope: this is what makes the merge non-transitive.
3. **Gate.** A bare name attributed to module `M` is visible iff one of these holds:
   - the executing compunit (or an `EVAL` parent of it) is `M`'s own compunit;
   - `M` is merged at unit level into that chain;
   - `M`'s merge key is live in the current env.

   #7797's qualified gate keeps its unit-level grants and also accepts a live block-level merge
   of a module that granted the package. An invisible name fails like an undeclared one: the
   `EVAL` pre-check reports `X::Undeclared::Symbols`, and a run-time bareword lookup throws the
   same.

## 3. Consequences

- The `need`/`use`-in-a-block, `need`-in-a-sub and through-a-module rows of §1.1 match Rakudo
  for bare names. The qualified rows match for block scoping.
- A `need`-only block now opens an import scope (it did not, since only `use`/`import`/`no`
  did).
- Instances, method dispatch, `.^name`, smartmatch against an already-obtained type object, and a
  module's own code are unaffected: the gate sits only on *name resolution* (bareword, indirect
  `::('...')`, bare routine call, the `EVAL` undeclared-name pre-check), never on the registry.
- Programs that relied on the leak break. This is the intended compatibility gain: Rakudo rejects
  them.

## 4. Known deviations (accepted)

- **Dynamic over-approximation inside the block.** The env is consulted dynamically, so a
  routine that runs while the block is live and executes on the caller's env may still see the
  names (Rakudo: not visible from a sub declared outside the block). This only widens visibility.
- **`GLOBAL::OuterCls`** stays resolvable: mutsu's `GLOBAL` *is* the shared store. This also
  only widens visibility.
- **A setting-namespace declaration** (`class X::Foo` in a module) is unaffected, as #7797
  already decided: Rakudo merges those into the setting's shared stash.

## 5. Alternatives rejected

- **Remove or rename the published entries when the scope closes.** Escaped instances, the
  module's own methods and a no-op re-`use` all need the entries. This is the same reason the
  stores are global today.
- **The process-global `suppressed_names` set** (the `my class` mechanism). It is not
  per-compunit, so hiding `OuterCls` after the importer's block would also hide it from
  `Outer`'s own methods.
- **Decide visibility at compile time from the parser's lexical scopes.** This is exactly
  lexical, but the parser knows module contents only from source scans, and every run-time
  lookup path (indirect names, `EVAL`, the bareword memo) would need a per-site annotation.
  The env tier already gives lexical capture, so the run-time gate reaches the same rows for
  far less.

## 6. Implementation status

- Bare-name provenance, merges and gate for types, packages and terms; block-level merges
  through import-scope env keys; `need`-only blocks open an import scope; the #7797 gate
  honours block-level merges (#11136's PR).
