# ADR-15760: NameScope — one scope type, source-order classification, and a typed node lowered where the parser builds it

- **Status**: Accepted (maintainer decision, 2026-10-31) after revision; Amendment 1 Accepted (maintainer decision, 2026-11-01), §6;
  Amendment 2 Proposed (2026-11-20), §7
- **Date**: 2026-10-31
- **Deciders**: tokuhirom, Claude
- **Issue**: [#15760](https://github.com/tokuhirom/mutsu/issues/15760) (RakuAST C2). Roadmap:
  [#7564](https://github.com/tokuhirom/mutsu/issues/7564).
- **Related**: [ADR-15334](15334-stage-2-parser-builds-typed-rakuast-nodes.md) (§2.1.2 and §2.3 state the goal this
  ADR makes decidable; Amendment 5 moved it out of Stage 2), [ADR-10723](10723-rakuast-is-the-frontend-ir.md),
  [ADR-0137](0137-typed-ast-visitor-for-analyses.md), [ADR-0026](0026-slang-activation-architecture.md)

This ADR supersedes nothing. It is the design ADR-15334 Amendment 5 asked for before C2 is implemented. The
maintainer's answers to its open questions (2026-10-31) are folded into §2 and §5.

## 1. Context

### 1.1 The executing path today (measured on `main` c4b04cd2, 2026-10-31)

An executing parse is three passes, not one:

```
 Source ─► Parser(keep_spelling) ─► Vec<Stmt> with Stmt::Ast / Expr::Ast / Expr::Spelled
        ─► frontend::round_trip_unit ─► finalize::finalize_unit   (src/rakuast/finalize.rs)
        ─► parser::qualify_type_names (src/parser/type_qualify.rs, 496 lines)
        ─► p6_frame_state::lower_program ─► the compiler
```

`finalize_unit` walks the whole unit after it is parsed: each typed node is lowered where it stands, each
`Expr::Spelled` is taken off (`unspell::strip`), and a construct the parser's tree cannot stand for is converted to
RakuAST and lowered on its own. Its name knowledge is collected **from the finished unit**:
`bareword::DeclaredNames::collect(&stmts)`, `shadowed_terms::scan_stmts`, `infix_func::enter_scope`. A unit is thus
classified against all of its own declarations at once, not the ones in scope at the point of use. The entry points
that are not a plain parse each seed the same knowledge differently: string `EVAL` (`round_trip_eval` +
`with_caller_names`, a thread-local `CallerNames`), `Str.AST` (`with_caller_trait_types`), and `EVAL($ast)` (`lower`,
which reads thread-locals the caller installed).

The parser, meanwhile, already keeps a live scope while it parses, as thread-local state (`SCOPES`, `PACKAGE_PATH`,
`INLINE_MODULE_EXPORTS`, the type and operator registers; 34 `thread_local!` sites under `src/parser`). It does not
hand lowering that scope; lowering re-derives a second, post-hoc one.

### 1.2 What this costs

- **Two sources of truth for names, and the post-hoc one is not rakudo's.** Measured with raku 2026.09 against
  mutsu (main 37affece):

  | Program | rakudo | mutsu |
  | --- | --- | --- |
  | `foo; sub foo { say "hi" }` | `hi` | `hi` |
  | `say A; class A {}` | `Illegally post-declared type` | `(A)` |
  | `sub f { A.new }; class A {}` | `Illegally post-declared type` | prints nothing, no error |
  | `say X; constant X = 3` | `(X)` (core `X`) | `3` |
  | `say x; my \x = 1` | `Undeclared routine: x` | `x` |

  Rakudo hoists **routines only**; packages, constants and sigilless terms are visible from their declaration on.
  The whole-unit pre-scan makes mutsu accept programs rakudo rejects and give a different answer where a core name
  is shadowed later. Issue #15791 tracks these rows.
- **A second tree walk and a second qualification pass** on every compilation unit. This is not mainly a speed
  matter: post-parse work is about 5% of the frontend, so the cost gate for this ADR is "does not rise", and the gain
  is compatibility (one classifier, rakudo's source order).
- **Spellings that exist only to be taken off**: `Expr::Spelled` is built by the parser on every executing parse
  and removed by `unspell::strip`.
- **Thread-locals as hidden parameters.** `CallerNames`, `CallerTraitTypes`, `with_caller_operators`, `DeclaredNames`,
  `shadowed_terms`, `infix_func` are installed around calls instead of passed to them.

### 1.3 What must not change

The safety invariants of ADR-15334 §2.4 hold: an analysis parse executes nothing; types, operators and slang modes
are registered when the node is built, in source order; the precompilation cache stores the lowered statements; no
new Rust file passes 500 lines.

## 2. Decision

### 2.1 One type owns the scope: `NameScope`

`NameScope` is a struct in a new `src/scope.rs`. It holds what lowering and classification need and today get from
the places in §1.1:

- the **type names** visible at a point (declared classes/roles/grammars/subsets/enums, imported types, caller types);
- the **term names** (constants, sigilless declarations, enum values, `my \x`);
- the **operator routines** (user infixes/prefixes/postfixes, imported);
- the **shadowed keywords** (a unit that declares `sub say`) and **declared routines**;
- a **parent link**, so a block's declarations leave scope when it closes.

It is queried by one classification function (the bareword classification of `bareword.rs`/`core_term_names` lives
in a single `NameScope::classify`), and seeded from another scope (`NameScope::from_caller`).

**Ownership.** Inside the parser, `NameScope` *is* the type of the parser's existing thread-local scope state: one
owner, the parser keeps reading and extending it where it does today, and the scattered tables (`SCOPES`,
`PACKAGE_PATH`, `INLINE_MODULE_EXPORTS`, the type and operator registers) become its fields as slices reach them.
There is no parser-wide signature threading. Lowering, string `EVAL`, `Str.AST` and `EVAL($ast)` receive it as an
**explicit parameter**; none of them reads a thread-local for names. It is not part of `Interpreter` (ADR-10779).

**Location.** `src/scope.rs` is fine. The layer question is not "parser names `rakuast`" (33 parser files already do
and `rakuast` is not a checked layer) but that `src/rakuast` references runtime and compiler in 8 files
(`lower.rs`/`convert.rs`/`meta_infix.rs`/`temporize.rs`/`hyper_prefix.rs` → `compiler::helpers_ops::{op_name_to_token_kind,
token_kind_to_op_name, prefix_op_name_to_token_kind}`; `bareword.rs` → `runtime::utils::is_known_type_constraint`;
`lower.rs` → `runtime::term_names::term_key`; `resolve.rs` → `Interpreter::is_builtin_function`; `mod.rs` →
`runtime::slang_activation`). Build-time lowering makes parser → lower → compiler/runtime a direct path, so the
prerequisite slice (§4, slice P) moves those helpers down, or makes `rakuast` a checked layer in
`scripts/check-layer-deps.py` with a shrinking baseline.

### 2.2 Who provides it, per path

| Path | Who builds and extends the `NameScope` | When a name is fixed |
| --- | --- | --- |
| Executing parse (program, module, fragment) | the parser, as it commits each statement | at the statement commit point (§2.3), in source order |
| String `EVAL` | the parser, seeded from the calling lexical scope: `NameScope::from_caller(…)` replaces `CallerNames::install` | as above |
| `Str.AST` | the parser in keep-spelling mode; the same seed | as above; the typed nodes are kept, not lowered |
| `EVAL($ast)` / hand-built tree | `lower`, which walks in statement order and extends a seeded `NameScope` through the same `NameScope::declare_*` methods the parser calls | during the walk |

### 2.3 Source order, and the commit point

**Classification is in source order, with no whole-unit pre-scan.** A type, constant or sigilless term is known from
its declaration on. Only **routine names are late-bound**: a bareword the scope does not resolve lowers to a
**deferred routine call**, resolved when the unit is complete, and `Undeclared routine` is reported at the end of the
unit if no routine of that name was declared (as rakudo's CHECK does). A use of a package name before its
declaration is the post-declared-type compile error rakudo gives, not a late-bound name.

**Backtracking.** The parser has about 220 alternation sites and snapshots scope state only around nested fragments
(3 sites). So lowering, and every `NameScope::declare_*`, happen at a **statement commit point** (when the statement
is complete), or the scope is cheaply restorable; they never run inside an alternative that can still fail. A
declaration inside an expression that is later abandoned must not leak into the scope.

### 2.4 Lower where built

For an executing parse, the parser calls the family's one lowering function at the commit point of the statement
that holds the typed node and stores the lowered `Stmt`/`Expr`. For a `Str.AST` parse it stores the typed node
(`Stmt::Ast` / `Expr::Ast`) as now. `Expr::Spelled` is built only by the `.AST` parse. The mode is one flag, as
`keep_spelling` is today.

### 2.5 What is deleted

When the last family is moved, in this order, each its own PR with the three-path parity test of ADR-15334 §3.1:

1. `unspell` (nothing is spelled on the executing path) and the `Expr::Spelled` wrapper in executing builds;
2. the second `qualify_type_names` pass, folded into node construction (types are qualified from the scope when a
   type node is built; it remains for the keep-spelling path only if `.AST` needs the bare name, as it does for
   `Term::Name`);
3. `finalize_unit` and the `frontend::round_trip*` names: `round_trip_unit` becomes `finish_unit` (the passes that
   truly run on a whole unit: the whatever-priming scopes, scope diagnostics, `p6_frame_state`, the end-of-unit
   routine check of §2.3);
4. the thread-locals `CallerNames`, `CallerTraitTypes`, `with_caller_operators`, `DeclaredNames` and the
   `shadowed_terms` / `infix_func` installs, replaced by `NameScope`.

`lower_converted`/`EVAL($ast)` keeps working throughout: it is the write direction and takes the same `NameScope`.

### 2.6 What `finalize` still converts, the cache, fragments

- **Fallback constructs.** `finalize_unit` still converts to RakuAST and lowers on its own: barewords to classify,
  regexes with code, operator declarations, packages holding a nested-block method, non-carried compound assignment,
  and `temp Type.meth = v`. Each needs either its own typed-lowering slice (the family becomes a `ParsedStmt` /
  `ParsedExpr` node like the migrated ones) or an explicit statement, in the PR that removes `finalize`, that
  `convert` + `lower` stays for it. This ADR does not decide which; removing `finalize_unit` (§2.5 item 3) is
  blocked until each is one or the other.
- **Precompilation cache.** The cache stores the lowered statements. Slice 1 changes what lowering emits (a deferred
  routine call instead of a classified name), so it bumps the cache format version; so does any later slice that
  changes the lowered tree.
- **Lazily parsed fragments** (ADR-15334 §2.1.2): where such a fragment exists (a block parsed on first use), it
  captures the `NameScope` at its position and lowers with that scope. Slice 2 audits for them.

## 3. Consequences

- **Fewer disagreements, by construction.** Execution, `EVAL` of its source and `EVAL` of its `.AST` classify a name
  with one function over one value, in rakudo's source order; the divergences of §1.2 are closed by slice 1.
- **Cheaper, or at least not dearer.** One walk and one qualification are removed from every unit; the spelled
  wrapper is not built. The gate is `scripts/frontend-cost.sh` "does not rise", not a speed-up.
- **A real behaviour change, isolated.** Slice 1 is the only behaviour-changing slice: programs that depended on the
  whole-unit pre-scan now get rakudo's error or answer. It is measured against raku and the roast suite.
- **Risk — the largest refactor left in the frontend.** Mitigation: structural and behavioural changes are separate
  PRs (§4), the family-by-family order of ADR-15334 §2.2 is kept, and `scripts/dev gate --full` plus the
  frontend-cost script gate each PR.
- **Risk — backtracking leaks.** A `declare_*` inside a failing alternative would leave a phantom name. §2.3's commit
  point rule is the control; each slice that adds a `declare_*` states where it is called.
- **Risk — slang activation.** A slang changes what the parser accepts mid-unit; its mode flags are parser state,
  not names, and stay where they are. ADR-0026's activation points register into the `NameScope` when they declare
  operators or types.

## 4. Slices

Structural slices change no behaviour; slice 1 is the only one that does. They are separate PRs.

- **P (prerequisite).** Move the 8 `src/rakuast` → runtime/compiler references down (or make `rakuast` a checked
  layer with a shrinking baseline).
- **0.** `src/scope.rs`: `NameScope` replaces `CallerNames` / `CallerTraitTypes` / `with_caller_operators` /
  `DeclaredNames` as an explicit parameter of `finalize_unit`, `round_trip_eval`, the `Str.AST` entry and `lower`. No
  behaviour change; the whole-unit pre-scan is still how the scope is filled.
- **1.** Source-order classification and routine-only late binding (§2.3), the end-of-unit `Undeclared routine`
  check, cache format bump. Closes #15791. **Split into 1a-1d by Amendment 1 (§6):** the mechanism and the cache note
  changed, the rule did not.
- **2.** Build-time lowering family by family (§2.4), deleting `finalize`'s share each time; fallback constructs
  (§2.6) and fragments audit; then the deletions of §2.5.

## 5. Alternatives considered, and the questions answered

- **Keep `finalize_unit`, only add `NameScope` as its argument.** This is slice 0, as the first step of this ADR, not
  the end state: alone it keeps the second walk and the spelled wrapper.
- **Make lowering read the parser's tables as a global.** Rejected by ADR-15334 §2.3: hand-built trees and `EVAL`
  strings have no parser, so a node would lower differently by path. (Inside the parser the scope *is* thread-local
  state, §2.1; lowering is handed it.)
- **Resolve names in a separate resolver pass over RakuAST (Rakudo's `resolve`).** `src/rakuast/resolve.rs` does this
  for the `$*CU` view. A pass over the finished tree is the post-hoc design this ADR replaces; the resolver stays a
  view for analysis tools.
- **Keep a whole-unit pre-scan for the hoisted forms.** Rejected (maintainer, 2026-10-31): rakudo hoists only
  routines, so only routines are late-bound.

Answers recorded (maintainer, 2026-10-31): source order with no pre-scan, only routine names late-bound (Q1); slice 0
first (Q2); `src/scope.rs` (Q3).

## 6. Amendment 1 (Accepted, 2026-11-01): where the early classification really comes from, and how slice 1 splits

Slice 0 is merged (#15837, #15840). The first attempt at slice 1 (branch `fix/15791-source-order-classification`:
`bareword::DeclaredNames::collect` and `BlockScope::enter` replaced by a per-statement `declare_statement`) built and
changed **none** of the rows of #15791. §2.3 assumed the frontend's whole-unit scan was the one source of the early
classification. It is one of five. The measurements below (raku 2026.09, mutsu at `main` after #15840, 2026-11-01) are
the correction.

### 6.1 The five sources

| # | Where | What it does today | Effect |
| --- | --- | --- | --- |
| 1 | `rakuast::bareword::DeclaredNames` / `BlockScope` (§1.1) | whole-unit and whole-block pre-scan, read by `bareword::convert` | classifies lowercase barewords in `finalize` and every bareword in the `.AST` view |
| 2 | `rakuast::carried::expr_carried`, `Expr::BareWord` arm | an uppercase bareword is **carried**: the frontend never classifies it on the executing path | it resolves at run time, where class declarations are registered before the unit's statements run: `say A; class A {}` prints `(A)` |
| 3 | `runtime::undeclared_routines` (`scan.rs`) | CHECK-time, scope-blind scan of the AST; `declared` deliberately absorbs variables, types and terms ("suppressing a call on any of them is the safe direction") | this is the "end-of-unit `Undeclared routine` check" §2.3 asks for, and it already exists; it excuses `say x; my \x = 1` because `x` is declared *somewhere* |
| 4 | `runtime::eval_post_declared_types` | by top-level statement index; called from `EVAL` only (`eval_check.rs`, `system.rs`) | the main program and `use`d modules have no post-declared-type check; `EVAL q[say A; class A {}]` already dies correctly |
| 5 | parser registries (`is_user_declared_type`, `register_user_*`) | filled when a node is built | already source order; not a cause |

Rakudo's `.AST` raises the same compile errors as running the code (`Q{say A; class A {}}.AST` is
`Illegally post-declared type`, `Q{say x; my \x = 1}.AST` is `Undeclared routine: x`); mutsu's `Str.AST` returns a tree.

### 6.2 Measured rows

| Program | raku | mutsu | Source |
| --- | --- | --- | --- |
| `say A; class A {}` | post-declared type | `(A)` | 2, 4 |
| `{ say B; class B {} }` | post-declared type | `(B)` | 2, 4 |
| `sub f { A.new }; class A {}` | post-declared type | nothing | 4 |
| `class A { method m { B.new } }; class B {}` | post-declared type | nothing | 4 |
| `say X; constant X = 3` | `(X)` (core `X`) | `3` | 2 (and run-time constant hoisting) |
| `say x; my \x = 1` | `Undeclared routine` | `x` | 3 |
| `{ say x; my \x = 1 }`, `say x; { my \x = 1 }` | `Undeclared routine` | `x` | 3 |
| `say Y; enum E <Y Z>` | `Undeclared routine: Y` | `Y` | 3 |
| `my \x = 1; { say x }` | `1` | `1` | — |
| `foo; sub foo {...}`, `{ foo; sub foo {...} }` | works | works | routines are hoisted |
| `{ sub foo {...} }; foo` | `Undeclared routine` | nothing | 3 (routine scoping; filed separately) |

### 6.3 Decision

§2.3's *rule* stands: source order, only routines late-bound. §2.3's *mechanism* ("lower an unresolved bareword to a
deferred routine call; report at the end of the unit") is replaced, because the end-of-unit check exists (source 3) and
the carried path (source 2) never produces an unresolved bareword to defer. Slice 1 is split into independent PRs:

- **1a. One post-declared-type check for every unit.** Replace `eval_post_declared_types` (EVAL only, statement-index) by a
  pure check over `&[Stmt]` and `&NameScope` that runs at the end of **every** executing parse (main program, module,
  `EVAL`) and in `Str.AST`: a type name used where no declaration of it is visible, but declared later in the unit, is
  `Illegally post-declared type`, including a use inside a routine or method body and inside a nested block. It lives in
  `src/rakuast` (a checked layer since slice P) as the `finish_unit` check §2.5 item 3 names. Closes rows 1-4 of §6.2.
- **1b. Order-aware term and type excuses in the undeclared-routine scan.** In `undeclared_routines::scan`, `declared`
  splits: routines stay hoisted unit-wide; variables, sigilless terms, enum values and types record the pre-order
  position of their declaration (and the lexical block they are visible in), and a bareword call is excused by one of
  them only when it is declared before the call in a block that encloses the call. The existing `Frame` for routine
  bodies is extended to blocks. Closes the `x` and `Y` rows. Includes the conservative `bail` cases unchanged.
- **1c. The `.AST` view in source order.** The frontend scan of source 1 becomes per-statement (the WIP branch
  is the starting point), and `Str.AST` runs 1a/1b so it raises what rakudo's `.AST` raises. `.AST` of
  `say X; constant X = 3` then renders the first `X` as the core `Type::Simple`.
- **1d (moves to slice 2). Classify uppercase barewords in the frontend instead of carrying them.** Row `say X; constant X = 3`
  executing as `(X)` needs the use to bind to the lexical symbol at its position, which no run-time resolution can do
  after hoisting. It needs `expr_carried` to stop carrying `BareWord`, so it belongs with the family-by-family lowering
  of slice 2. #15791 stays open for this row only.
- **Precompilation cache.** 1a-1c do not change the lowered statements, so the cache format is **not** bumped by them
  (this replaces the note in §2.6); 1d will bump it.

### 6.4 Consequences and risks

- 1a and 1b make programs fail that mutsu runs today. That is the point (rakudo rejects them), but it is the largest
  roast risk of the campaign: every roast file that forward-references a type or term is affected, so each PR runs
  `make roast` in full and records the files it moves.
- 1a must not reject what rakudo accepts: `role R {...}`/`class C {...}` stubs declare at the stub; `unit` declarations
  and `our`-scoped nested names (`module M { our class N }; M::N`) are visible by qualified name from the start of their
  declaration; imported types (`use`, `need`, `import`) are declared at the import statement.
- Finding filed separately: `{ sub foo {...} }; foo` runs silently in mutsu (rakudo: `Undeclared routine`).

### 6.5 Questions for the maintainer (answered 2026-11-01)

1. `Str.AST` raising the two compile errors (as rakudo does) rather than returning a tree: recommended yes (1c).
2. Row `say X; constant X = 3` deferred to slice 2 as 1d, #15791 left open for it: acceptable?
3. The post-declared-type check in `src/rakuast` rather than `src/runtime`: it needs no interpreter, and `src/analysis` /
   the language server can call it (ADR-0065); recommended yes.

Answers (maintainer, 2026-11-01): 1 yes, 2 yes, 3 yes, as recommended. Amendment 1 is Accepted.

## 7. Amendment 2 (Proposed, 2026-11-20): what is left of slice 2, the converted allow-list, and the commit point

### 7.1 Where the campaign stands (`main` after #18268 and the no-spellings PR)

Done from §2.5: the `frontend::round_trip*` names are gone (`finish_unit`, `finish_unit_in`, `finish_eval_unit`;
#18268), and an executing parse builds no `Expr::Spelled` (`unspell` deleted; the one spelling the executing path
read, `Spelling::ImportedTerm`, is the typed node `ParsedExpr::ImportedTerm`; the `*_spelled` parse entry points are
`*_unfinalized`). Slice 2 has carried every `ParsedExpr` family and most `Expr` kinds through `finalize_unit`
(#16257 .. #16710).

What is left is the second walk itself. An executing unit is still parsed whole, then walked by
`finalize::finalize_unit`, then walked again by `parser::qualify_type_names` (`type_qualify.rs`), then by
`p6_frame_state::lower_program`. §2.4 says the parser lowers at the statement commit point; two things stand in the
way, and both are inputs, not lowerings:

1. **Whole-unit pre-scans feed the finalization.** `bareword::DeclaredNames::collect(&stmts)` (every name the unit
   declares, for the bareword classifier), `finalize::unit_type_names` (every type the unit declares),
   `shadowed_terms::scan_stmts` (a `sub True` / `sub say` anywhere in the unit) and
   `underscore_bindings::Scope::block(&stmts)`. At a statement's commit point the statements after it are unknown.
2. **The qualification pass needs a list's later statements.** `type_qualify::later_core_types` pins a core type name
   used before a later `my class DateTime` in the same list to `CORE::DateTime`; its `PackageTypes` pre-scan collects
   the `our` types declared anywhere in the unit.

Two independent censuses of the fallback (worker 12: 181 hits in 47 of 1954 files; worker 13: 122 hits in 51 of
1970) found no construct family above 8 files: there is no family whose typed-lowering slice empties the fallback.

### 7.2 Decision proposed

**(a) §2.6 is settled as option (b): `convert` + `lower` stays, by name, for the constructs whose RakuAST node is
the only place their executable shape is computed.** The list is exactly what `rakuast::carried` refuses to carry
(`stmt_carried` / `expr_carried` returning `false`), and it is closed: a construct leaves it when its own slice lands,
and nothing joins it without an amendment.

| Construct | Why the conversion computes it |
| --- | --- |
| an operator routine (`sub infix:<+>` with `is assoc` / `is equiv` / `is tighter`, a symbolic name, signature alternatives) | the precedence and associativity are read off the node by class |
| a method with a computed name, a value prototype, `is DEPRECATED`, or declared in a nested block of a package body (and the package holding it) | the conversion of the package places the method and closes it over the block |
| `temp Type.meth = v` | the temporizer's save / assign / restore expansion |
| a compound assignment with a scalar `Z`/`X`/hyper meta (`$x Z+= @y`) | the meta-infix node is the only reader |
| a literal `Signature`, the deferred `X::Obsolete`, a regex literal outside a declaration, a sub value | the value is built (or the compile error raised) by the conversion |
| an interpolating quote-form `s///` replacement | becomes the per-match thunk in the conversion |
| a `\|\|` dimension splat, a pointy block with a custom trait, a qualified or operator code variable | spelled forms the lowering rewrites |
| a bareword the classifier must place (`decode`, a label, a term the unit declares) | the classification itself (§2.3) |

These are lowered by `convert` + `lower` **at the same commit point as everything else** (below), not by a separate
pass.

**(b) The commit point of the finalization is the close of a statement list.** When the parser completes a statement
list (a block, a routine or package body, the unit), it finalizes that list: the list's statements are lowered in
order, each nested list having been finalized when it closed. §2.3's backtracking rule holds unchanged: a list that
is complete is no longer inside an alternative that can fail (a block re-parsed by a later alternative is finalized
again from its fresh parse; finalization is a pure function of the list and the scope). The per-statement commit
point of §2.4 is replaced, because every input the finalization needs per list (`later_terms`, `BlockScope`,
`declares_routine`, `later_core_types`) is known exactly when the list closes and not before.

**(c) The whole-unit pre-scans become "declared so far", except routines.** At a list's close the finalization sees
the declarations of the enclosing lists made before it (source order, as rakudo) plus its own list. A **routine**
declared later in an enclosing list is the one thing it cannot see; it needs nothing: an unclassified lowercase
bareword lowers to the word itself (what `bareword::call_word_expr` already produces for a declared routine), which
the run time calls, and the existing end-of-unit scan (`runtime::undeclared_routines`, §6.1 source 3) reports
`Undeclared routine` when no routine of that name exists. A type used before its declaration is already the
post-declared-type error of slice 1a; a term is source-order by rule. A unit-wide keyword shadowing (`say 1; sub say
{...}`) is the one open case: rakudo hoists the `sub`, so the earlier `say` calls it. The lowered `say 1` is the same
call by name either way; the census of the slice that moves `shadowed_terms` decides whether anything else depends
on it.

**(d) Type qualification folds into the same list finalization.** The `Qualify` state (open packages, the list's
`my` classes and block `use` imports, the pinned core names) becomes part of the finalization context, and each
lowered statement's type positions are qualified as it is finalized. `PackageTypes` becomes the `our` types declared
so far. `parser::qualify_type_names` remains only for `EVAL($ast)` (a hand-built tree has no parser).

**(e) `finalize_unit` and the frontend's `qualify_type_names` call are then deleted**; `frontend::finish_unit` keeps
the passes that truly run on a whole unit (`whatever_curry::mark`, the scope diagnostics, `p6_frame_state`).

### 7.3 Slices

- **2a (structural).** The finalization is driven by list close: `stmtlist` hands each completed list to the
  finalization context, which still holds the whole-unit pre-scans (computed from the unit as parsed so far plus an
  end-of-unit pass for what is not yet known). No behaviour change; `finalize_unit` becomes the driver of the unit's
  own list only.
- **2b (behavioural).** The pre-scans become "declared so far" (§7.2 (c)); measured against raku and the full roast.
- **2c (structural).** Qualification folded in (§7.2 (d)).
- **2d.** `finalize_unit` and the executing `qualify_type_names` call deleted; frontend cost measured against
  Amendment 1 of ADR-15334.

### 7.4 Questions for the maintainer

1. §2.6 settled as the closed allow-list of §7.2 (a) (worker 19's working reading, #15760): accepted?
2. The commit point is the close of a statement list, not each statement (§7.2 (b)): accepted?
3. Slice 2b changes behaviour only for programs that refer to a routine-shadowed keyword before the `sub`, or that
   rely on whole-unit type knowledge rakudo does not have; it runs the full roast. Acceptable as a separate
   behavioural PR?
