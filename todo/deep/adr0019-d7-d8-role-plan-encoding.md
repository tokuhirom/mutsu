# ADR-0019 D7/D8 design: typed role structure and compiled deferred bodies

Design pass (2026-08-08, no code landed) for D7 ("Encode role structure and composition —
parameters, attributes, methods, parent roles, conflicts, hides, pun metadata — into immutable
plan operations") and D8 ("Compile role declaration-time bodies and traits — run
parameterized-role and composed ancestor bodies as bytecode child chunks with correct
once-per-composition behavior"). D8 also closes D4's "deferred class bodies" piece (same data,
same entry points — established by the D4 scoping pass) and unblocks D9.

## Survey facts (2026-08-08)

- `register_role_decl`'s passes and what is already plan-driven: `type_params`/
  `type_param_defs`, `own_attribute_names`, `body_used_modules`, `body_declared_types` (D2a),
  `method_name_chunks`/`method_decls` (D3-1/D3-7), `custom_traits` as `DeclTraitArg` chunks
  dispatched by the VM op tail (`vm_typedecl_ops.rs:716-744`). Still AST-driven: the
  our-scope/stub pre-scans, the `HasDecl` arm (runtime `from_stmt`), the `DoesDecl` arm
  (string-keyed parent resolution, hides/hidden markers), and the catch-all that clones every
  remaining statement into `RoleDef::deferred_body_stmts` (`registration_role_decl.rs:240-251`)
  — for parametric and plain roles alike; **nothing from a role body executes at declaration**.
- Per-arity candidates live in `registry.role_candidates: HashMap<String,
  Vec<RoleCandidateDef>>` (replace-or-append by signature match,
  `registration_role_decl.rs:299-319`); `resolve_role_candidate` selects by arity filter +
  trial bind + specificity score. Conflicts: attribute conflicts detected during the role walk
  and raised at composition; method conflicts at `class.rs:190-280`; required-method checks at
  `registration.rs:280-395`. `hides` is a `__mutsu_role_hides__` pseudo-parent marker →
  `registry.role_hides`; `is hidden` a marker → `RoleDef.is_hidden`.
- Deferred bodies execute per composition via `run_block_raw` **one statement at a time** at
  five consumer sites (class-header composition `run_composed_role_deferred_body`
  (`registration_class_compose_body.rs:64-277`), `also does`, puns, runtime mixins, and the
  shared `run_role_body_for_composition` (`registration_class_augment.rs:1258-1303`)), with:
  per-statement package routing (nested type decls → the **role's** package, `token`/`rule` →
  the **composing class's** package), type captures bound at env level (`bind_type_capture` —
  markers removed after, names kept), a `VarDecl` re-scan to persist body lexicals as
  composing-class statics, per-composition renaming of nested classes
  (`rename_generic_composed_class`), and `X::Role::Instantiation` wrapping of a dying body.
- Once-per-composition tracking is partial: `registry.composed_role_bodies` memoizes only
  `pun:{role}` and `mixin:{role}` (role-global, not per target); the class-composition path has
  **no guard** — re-registering the same class re-runs the role body each time.
- `run_role_submethod` (`types/roles.rs:516-577`) — the C6d-3 leftover — still executes a
  mixin BUILD/TWEAK `MethodDef` body via `eval_block_value`.
- The chunk mechanism exists: `CompiledDeclExpr` compiles any `&[Stmt]` slot-free and runs
  re-entrantly via `run_decl_expr` mid-registration (C5); only its current constructor is
  expression-shaped.

## D7 design — typed role structure

`CompiledRoleDeclPlan` gains, mirroring the class side's D1/D2/D6 fields:

- `is_stub: bool` and `our_scope_violation: Option<&'static str/kind>` — the two pre-scans
  become plan facts (shared with D9-1).
- `attr_decls: Vec<(Symbol, CompiledAttrDecl)>` name-keyed (D2b-2's role half).
- `parent_ops: Vec<RoleParentOp>` — one per `does` (including the parser's synthetic
  `DoesDecl`s, which today are the only carrier of role parents): `{ name: Symbol, hides:
  bool, hidden: bool, args: Vec<DeclTraitArg> }`. This replaces the `__mutsu_role_hides__`/
  `__mutsu_role_hidden__` string-marker encoding with typed flags and gives D4's bracket-arg
  chunks their role-side carriage (the class-header half is D4-2's `parent_arg_chunks`).
- `body_plan: Vec<RoleBodyOp>` — the ordered walk list (Attr/Method/Parent/Deferred), same
  shape as D6's `ClassBodyOp`, so `walk_role_body` becomes an op executor.

What deliberately stays runtime: candidate selection, trial binding, specificity scoring,
conflict/required-method detection, and pun materialization — these read the *registry* (other
roles, the composing class), not the AST, and are composition-time by nature. D7's claim is
narrower than the box text sounds: the *declaration's own structure* becomes immutable plan
data; the *composition algebra* over that data stays where it is. Pun metadata needs no new
encoding — `ensure_role_punned_to_class` copies registry data only; once `deferred_body_stmts`
is chunk-backed (D8), nothing the pun copies is AST.

## D8 design — deferred bodies as per-statement chunks

**Unit of compilation: one chunk per deferred statement, not one chunk per body.** The
consumers' per-statement package routing, the statement-kind dispatch
(`ClassDecl`/`RoleDecl` vs `TokenDecl`/`RuleDecl` vs plain), and the lexical-persistence scan
all operate at statement granularity; a monolithic body chunk would have to reproduce package
switching *inside* the chunk. So:

```
RoleDef::deferred_body: Vec<DeferredBodyOp>   // replaces deferred_body_stmts
DeferredBodyOp = { chunk: CompiledDeclExpr,
                   kind: TypeDecl | TokenRule | Plain,
                   declared_vars: Vec<Symbol>,   // replaces the VarDecl re-scan
                   raw: Option<Stmt> }           // token/rule rump, same rule as D6
```

lowered at plan compile time (the ops live on `CompiledRoleDeclPlan`; `finish_role_registration`
moves them onto `RoleDef` for the registry, replacing the raw clone). The five consumer sites
keep their exact env dance — type-capture binds, package save/switch/restore per op using
`kind`, `X::Role::Instantiation` wrapping, static persistence from `declared_vars` — swapping
only `run_block_raw(stmt)` for `run_decl_expr(chunk)`. What D8 removes is the per-composition,
per-statement throwaway compile; what it must not change is any composition-visible behavior.

**The frozen-plan question (the survey's caveat, resolved as a verification item, not a
blocker).** Compiling deferred statements at role-declaration time freezes any nested
declaration's *plan* per role, whereas today each composition re-lowers it from raw AST. But
registration is per-*execution*, not per-compile: running the chunk executes its `RegisterDecl`
ops afresh each composition, composition-dependent names (parent types, type captures) resolve
at execution through the env exactly as the D2c research pass established for defaults, and the
parametric rename pass operates on the registry after the run. So freezing the plan should be
semantics-preserving — **V1**: verify with a parametric role whose body declares a nested class
referencing `T` (`role R[::T] { class Inner { has T $.x } }`) composed at two different type
arguments, against `raku`. If a case genuinely needs per-composition re-lowering, that op keeps
a `raw` fallback (guarded-degrade, same pattern as everywhere else).

**Once-per-composition semantics are preserved, not "fixed".** The ADR text's
"correct once-per-composition behavior" is read as: keep today's observable behavior (unguarded
class-path re-runs on re-registration; role-global `pun:`/`mixin:` memos) while making each run
cheap. Whether the memo *should* be `(role, target)`-keyed is a raku-conformance question
independent of the chunk migration — **V2**: build a small case table against `raku`
(loop-redeclared class composing a side-effecting role body; same role mixed into two values;
two classes composing one role) and file any divergence as its own ticket rather than folding a
behavior change into D8.

**D8 rider — `run_role_submethod` goes bytecode.** After D3-8, a role's BUILD/TWEAK
`MethodDef` carries `compiled_code`; rewire the mixin path's `eval_block_value(&def.body)` to
run it (keeping the captured-env merge and `!attr` seed/readback), retiring the last
Phase-D-assigned interpreter body-execution site.

## Slices

- **D7-1** = D9-1 (role `is_stub` + our-scope plan facts). **D7-2** = D2b-2's role half.
  **D7-3** — `parent_ops` (typed does/hides/hidden + D4 arg chunks) consumed by
  `role_body_does_decl`; the marker-string encoding retires. **D7-4** — `body_plan` op walk
  (additive, instrument-gated like D6-3).
- **D8-1** — lower `DeferredBodyOp` chunks; `finish_role_registration` stores them; consumers
  still read raw stmts (additive). **D8-2** — consumer cutover behind the V1/V2 case tables +
  roast S14 + battery gate (Cro/OO::Monitors exercise parametric composition heavily).
  **D8-3** — the `run_role_submethod` rider. **D8-4** — drop `deferred_body_stmts` (the raw
  vec), leaving only token/rule raws per the D6/D9 rump rule.

Sequencing with the rest of Phase D: D7-1/2 are independent; D7-3 wants D4-1 (parser `Expr`
capture) first; D8 wants D3-8 (for D8-3) and feeds D9-4/D9-5. Full ordering in
`todo/deep/adr0019-d6-d9-legacy-body-removal.md`.
