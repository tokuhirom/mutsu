# ADR-15760: NameScope — one scope type, source-order classification, and a typed node lowered where the parser builds it

- **Status**: Accepted (maintainer decision, 2026-10-31) after revision
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
  check, cache format bump. Closes #15791.
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
