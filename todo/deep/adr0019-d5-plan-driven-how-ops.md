# ADR-0019 D5 design: user HOW operations are already plan-compatible — D5 is ordering invariants plus verification

Design pass (2026-08-08, no code landed) for D5 ("Drive user HOW operations from plan ops.
Execute `new_type`, `add_method`, trait interception, and `compose` without entering
`register_class_decl`'s AST walker"). The survey's central finding **shrinks this box**: the
user-HOW protocol does not interleave with the AST body walk at all, and no HOW call-out reads
a raw `Stmt`. D5 is therefore not a migration of the HOW machinery — it is a set of ordering
invariants D6 must preserve, plus a verification gate.

## Survey facts (2026-08-08)

- A custom declarator (`monitor Foo {...}` via `EXPORTHOW::DECLARE`) is an ordinary
  `Stmt::ClassDecl` tagged with a `__mutsu_declare_how` marker in `custom_traits`
  (`parser/stmt/class/class_decl.rs:160-176`); the plan carries the keyword as a
  `DeclTraitArg::Literal` (`opcode.rs:2127`). The HOW *type* is resolved at execution time from
  the env (`EXPORTHOW::DECLARE::<kw>`, `vm_typedecl_ops.rs:439-440`) — runtime-only by nature.
- **The protocol runs entirely after native registration**: `register_class_decl` (shell →
  body walk with direct registry writes → finalize → `install_class_exporthow`) returns, then
  the VM op does: DECLARE HOW attach + `declare_drive_how_protocol`
  (`vm_typedecl_ops.rs:432-451`) → class-level `trait_mod:<is>` dispatch from plan fields
  (`:456-500`) → user `compose` drain (`:507-514`, queued via
  `registry.pending_class_compose`).
- `new_type` marshals only the class name (with `pending_declare_new_type` backing the user's
  `callsame`, `metamodel.rs:388-399`); `add_method` **re-enumerates from the finished
  registry**, not the AST (`metamodel.rs:401-452` — public, non-submethod, non-multi first
  overloads, name-sorted, `Method` objects built by `make_method_object_with_owner` with
  `__mutsu_lookup_*` markers so user `.wrap`s land in `method_wrap_chains`); `add_attribute`
  is never called by mutsu (the user's `new_type` calls the native bridge itself); `compose`
  needs only the type object and the stored HOW instance (`registry.class_how_values`,
  `registry.rs:122`).
- Trait interception inputs are plan-resident already: class traits are plan fields; method
  traits/bodies live on `CompiledMethodDecl` (`custom_traits` at `opcode.rs:409`, `body` at
  `:397` — the walker builds the code object from the decl, `registration_class_body_method.rs:
  237-299`); attribute traits ride `CompiledAttrDecl` (the one remaining runtime `from_stmt`,
  closed by D2b-2).

## What D5 actually consists of

**D5-1 — codify the ordering invariants as the contract D6's body-plan cutover must keep.**
The load-bearing sequence: shell publish → body registration (direct registry writes,
re-published per statement) → HOW instantiate (`install_custom_class_how` before trait
dispatch, so traits reach the HOW) → `new_type` → `add_method` → class `trait_mod:<is>` →
`compose` (which must observe trait side effects, e.g. `@!aspects`,
`vm_typedecl_ops.rs:502-506`). Two structural rules for any plan-driven registration:
(a) **the registry stays authoritative between steps** — user code (attribute traits
`^add_method`ing onto the class mid-registration) mutates the registry, and the walker's
merge-back (`registration_class_body_attr.rs:167-185`) + per-statement re-publish
(`registration_class_body.rs:227-230`) exist precisely so direct writes and side effects
interleave; a plan executor must not batch into a private `ClassDef` clobbered at the end.
(b) **HOW installs key on the resolved storage name** (lexical mangling is per-execution,
`vm_typedecl_ops.rs:186-202`), never the plan's static name. This slice is documentation plus,
optionally, one mechanical move for coherence: the DECLARE-keyword attach
(`vm_typedecl_ops.rs:432-451`) migrating next to its sibling `install_class_exporthow` in the
D0 exit-phase file — behavior-neutral, low value, skip if it churns.

**D5-2 — verification gate, run after each D6 body-plan slice.** The box's completion
criterion: the HOW protocol behaves identically when the registry is populated from plan ops
instead of the AST walk. Since `add_method` reads the finished registry, this is automatic *if*
the registry ends up identical — so the gate is behavioral: OO::Monitors verbatim
(`scripts/battery-testsuite.sh`, the EXPORTHOW::DECLARE campaign's own acceptance bar) plus the
metamodel roast files, executed as part of D6's slice verification rather than as a separate
code PR.

## Consequence for the ADR

D5 has no independent implementation campaign; recommend re-scoping the box text to "preserve
and verify" (D5-1 + D5-2) and sequencing it as a rider on D6. If D6's instrumentation survey
later finds a HOW call-out that *does* read AST (none found in this pass), it re-opens as a real
migration slice.
