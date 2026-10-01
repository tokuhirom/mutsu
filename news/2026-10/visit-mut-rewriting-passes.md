# Rewriting passes walk the AST through `VisitMut`

ADR-10499 is implemented: `src/ast_visit/` now has a mutable visitor, `trait VisitMut`, whose
`walk_*_mut` functions mirror the read-only walkers child for child (every variant and field
named, no `_ =>`). Statement lists go through one extra hook, `visit_stmts_mut`, so a pass that
needs a body as a list (an ordered scope walk, a lift that edits the list) overrides just that.
`walk_param_mut` gives a rewritten parameter a fresh `ParamCode` slot (ADR-0133).

Three rewriting passes are ported (192 → 182 hand-rolled walkers). Each one used to skip whatever
its `_ =>` arm did not list. Every position it now reaches was checked against rakudo:

- **Outer-redeclaration / self-initializer check** (`parser/outer_redecl/`). It now sees reads in
  parameter defaults and `where` clauses, proto bodies, variable `where` clauses, `s[...] = ...`
  thunks, subset predicates and `temp` targets, so `my $y = 1; sub f($a = $y) { my $y }` is
  `X::Redeclaration::Outer`, as in rakudo. Attribute defaults and regex code blocks are nested
  scopes, and a code parameter's signature is never run, so all three stay accepted. `whenever`
  parameters are now the block's own lexicals, which removes a false `X::Redeclaration::Outer`.
- **Nested-BEGIN lift** (`runtime/begin_prologue/nested/walk.rs`). A `BEGIN` in a phaser body, a
  C-style loop header, a parameter default, a trait argument or any operator operand used to run
  late or not at all. It now runs in the unit prologue.
- **INIT/CHECK lift and per-level reorder** (`runtime/phasers/lift.rs`). An `INIT`/`CHECK` in a
  method body, an `ENTER`/`LEAVE` body, an element assignment, a feed or a compound assignment
  now runs before the mainline.

A lift must not reach a node that only copies code the compiler runs. If it did, the phaser
would run twice. The lifts therefore skip the `target`/`rhs` of a `CompoundAssign`, which copy
`expanded`, and regex trees, whose code blocks may run from another copy (#10550). Each of these
stops is an explicit hook override in the pass. The remaining differences from rakudo are filed
as separate issues: phasers in parameter defaults (#10551), INIT/CHECK in type and package bodies
and in prologue routines (#10552), and three scope-check positions (#10553).
