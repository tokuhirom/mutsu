# ADR-17472: A package node declares its type ahead of EVAL, and EVAL adopts it

- **Status**: Proposed
- **Date**: 2026-11-12
- **Issue**: [#17472](https://github.com/tokuhirom/mutsu/issues/17472) (EEL 0.3's `eel -e`)
- **Related**: ADR-10723 (RakuAST is the frontend IR), ADR-13370 (slang-installed classes)

## 1. Context

In rakudo a package declaration's type object exists from the moment its
`RakuAST::Class` (`Role`, `Grammar`, `Package`, `Module`) is built: the
declaration's BEGIN-time work creates and composes it while the source is
parsed. So `'class Foo { method hi { "hi" } }'.AST` already holds a usable
`Foo`: `$node.meta-object.new.hi` works before `EVAL`, a
`.HOW.set_name($type, ...)` made then is what the later `EVAL` declares, and
`EVAL` composes no second type. EEL's CLI (`EEL::CLI::run-cli`) relies on all
three: it renames each inline event type before `$ast.EVAL`, and reads
`.meta-object` again afterwards to register the same types.

mutsu has no BEGIN-time phase for a tree. `.AST` only parses and converts; a
package is declared when the lowered unit runs (`RegisterClass` and friends).
`meta-object` of a package node had no row at all and fell through to the
parameter row, whose failed lowering surfaced as "EVAL does not yet support
lowering RakuAST::Class".

## 2. Decision

1. **`.AST` remembers its unit.** The `RakuAST::Origin::Source` instance every
   origin of a parsed unit shares carries the top-level statement list `.AST`
   returned, as a hidden attribute.
2. **`meta-object` of a package node declares the type on demand.** Answered in
   order:
   - the type this node already declared ahead (step 3), recorded on its
     source by the node's start offset;
   - else the type its name resolves to in the calling package (the node was
     `EVAL`ed already);
   - else the declaration runs now: a unit of the top-level `use` / `need` /
     `import` / `no` statements that precede the node's statement in its
     remembered unit, followed by that statement, is lowered and run like any
     tree `EVAL` in the calling context. The type it yields is recorded on the
     source under the node's offset.
3. **`EVAL` adopts a type declared ahead.** Lowering a package node whose
   source records a type for its offset does not declare it again: the
   statement becomes the type object itself (the value the declaration would
   have produced). A rename made in between (`set_name`) survives because the
   type is the same object.

## 3. Consequences

- `meta-object` before `EVAL` returns a composed, usable type, and the type is
  identical before and after `EVAL` (`===`), as in rakudo.
- The declaration runs in the scope of the ahead-of-time unit: its methods
  close over that unit, not over the later `EVAL`. A method that reads a
  `my` variable declared earlier in the same unit fails to compile when
  declared ahead (it is not part of the declaration unit). rakudo has a
  BEGIN-time lexpad for the whole unit; mutsu does not, so this is the
  remaining gap (TODO in the code).
- Only a top-level package statement of a parsed unit is declared ahead. A
  package nested in another package or a routine, or a hand-built node
  without an origin, keeps answering only after `EVAL` (by name).

## 4. Alternatives

- **Run the whole unit's declarations at `.AST` time**, as rakudo's BEGIN
  does: needs mutsu to split every unit into a BEGIN-time and a run-time
  phase. That is the long-term shape (ADR-10723), but far larger than this
  gap; the on-demand declaration is a subset of it and would be replaced by it.
- **Return a stub type and let `EVAL` fill it** (mutsu keys types by name, so
  the stub and the declared class would be one name): a stub is not composed,
  so `.^isa`, `.new` and `.^methods` before `EVAL` would all be wrong, and
  EEL tests `.^isa(Event)` before it renames.
- **Answer an anonymous stand-in**, as `Routine.meta-object` does for a routine
  before it is declared: loses the identity across `EVAL` that EEL needs.

## 5. Amendment (2026-11-12, #17522): the unit's earlier lexicals

The gap §3 recorded — a method of a package declared ahead could not read a
`my` declared earlier in its unit — is closed without a BEGIN-time phase:

- `.AST` also records the unit's top-level `my`/`our` declarations (name and
  start offset) on the unit source.
- Declaring a package ahead adds those declarations to the ahead unit. Each
  `my` is bound there to a shared container (a `ContainerRef` cell holding
  `Any`, an empty `Array` or an empty `Hash`), recorded on the unit source by
  the declaration's offset; the package's methods close over it.
- When the unit is `EVAL`ed, its own declaration of such a variable lowers to
  a bind of that same container followed by the assignment of its initializer
  through it (`declared_ahead_vars::rewrite`). The methods then read what the
  `EVAL` stored, as in rakudo; before the `EVAL` they read an undefined value,
  as rakudo's BEGIN-time lexpad does.
- An `our` is declared again in the ahead unit, so the package compiles, but
  is not shared yet (binding the package symbol to a container loops through
  the package store); that remains a TODO.
