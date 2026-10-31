# ADR-15760: NameScope — the parser owns the scope, and a typed node is lowered where the parser builds it

- **Status**: Proposed
- **Date**: 2026-10-31
- **Deciders**: tokuhirom (to decide), Claude (author)
- **Issue**: [#15760](https://github.com/tokuhirom/mutsu/issues/15760) (RakuAST C2). Roadmap:
  [#7564](https://github.com/tokuhirom/mutsu/issues/7564).
- **Related**: [ADR-15334](15334-stage-2-parser-builds-typed-rakuast-nodes.md) (§2.1.2 and §2.3 state the goal this
  ADR makes decidable; Amendment 5 moved it out of Stage 2), [ADR-10723](10723-rakuast-is-the-frontend-ir.md),
  [ADR-0137](0137-typed-ast-visitor-for-analyses.md), [ADR-0026](0026-slang-activation-architecture.md)

This ADR supersedes nothing. It is the design ADR-15334 Amendment 5 asked for before C2 is implemented: nothing in
`src/` changes until it is `Accepted`.

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
classified against all of its own declarations at once, not the ones in scope at the point of use; and the three
entry points that are not a plain parse each seed the same knowledge differently: string `EVAL`
(`round_trip_eval` + `with_caller_names`, a thread-local `CallerNames`), `Str.AST` (`with_caller_trait_types`), and
`EVAL($ast)` (`lower`, which reads thread-locals the caller installed).

The parser, meanwhile, already keeps a live scope while it parses: declared types, `use`d modules' exports, user
operators, slang modes (`parser::with_rakuast_lexical_scope`, `register_*`). It simply does not hand lowering that
scope; lowering re-derives a second, post-hoc one.

### 1.2 What this costs

- **Two sources of truth for names.** A name the parser knew at node-build time and `DeclaredNames::collect` finds
  later can differ (shadowing, a declaration after its first use, a declaration in a block that has closed). Each such
  difference is an `.AST`-vs-execution disagreement that ADR-15334 §2.3 calls out: "whether a program, its `EVAL`
  string and `EVAL` of its `.AST` mean the same thing is a property of one function, not of three code paths".
- **A second tree walk and a second qualification pass** on every compilation unit (frontend cost: ADR-15334
  Amendment 1 reports zef-cli at 104.3%, grammar-cursor at 108.1% of the pre-flip frontend; the walk is part of the
  remainder).
- **Spellings that exist only to be taken off**: `Expr::Spelled` is built by the parser on every executing parse
  and removed by `unspell::strip`.
- **Thread-locals as parameters.** `CallerNames`, `CallerTraitTypes`, `with_caller_operators`, `DeclaredNames`,
  `shadowed_terms`, `infix_func` are installed around calls instead of passed to them.

### 1.3 What must not change

The safety invariants of ADR-15334 §2.4 hold: an analysis parse executes nothing; types, operators and slang modes
are registered when the node is built, in source order; the precompilation cache stores the lowered statements; no
new Rust file passes 500 lines.

## 2. Decision (proposed)

### 2.1 One type owns the scope: `NameScope`

`NameScope` is a struct in a new `src/scope.rs` (below `parser`, `rakuast` and `compiler` in the layer order of
`scripts/layer-deps-baseline.txt`; it names neither). It holds what lowering and classification need and today get
from the places in §1.1:

- the **type names** visible at a point (declared classes/roles/grammars/subsets/enums, imported types, caller types);
- the **term names** (constants, sigilless declarations, enum values, `my \x`);
- the **operator routines** (user infixes/prefixes/postfixes, imported);
- the **shadowed keywords** (a unit that declares `sub say`) and **declared routines**;
- a **parent link**, so a block's declarations leave scope when it closes.

It is an ordinary value: pushed and popped by whoever walks, queried by one classification function (the bareword
classification of `bareword.rs`/`core_term_names` lives in a single `NameScope::classify`), and cloned or seeded from
another scope. It is not a global, not a thread-local, and not part of `Interpreter` (ADR-10779).

### 2.2 The parser is the provider on the executing path, `lower` is the provider on the others

| Path | Who builds and extends the `NameScope` | When a name is fixed |
| --- | --- | --- |
| Executing parse (program, module, fragment) | the parser, as it builds each node (the tables it already keeps move into the `NameScope`) | at the moment the node is built, in source order |
| String `EVAL` | the parser, seeded from the calling lexical scope: `NameScope::from_caller(…)` replaces `CallerNames::install` | as above |
| `Str.AST` | the parser in keep-spelling mode; the same seed | as above; the typed nodes are kept, not lowered |
| `EVAL($ast)` / hand-built tree | `lower`, which walks in statement order and extends a seeded `NameScope` through the same `NameScope::declare_*` methods the parser calls | during the walk |

The lowering functions take `&NameScope` (and `&mut` where the node declares). A declaration or `use` extends it
before the next node is built or lowered.

### 2.3 Lower where built

For an executing parse, the parser calls the family's one lowering function at the point it builds the typed node
and stores the lowered `Stmt`/`Expr`. For a `Str.AST` parse it stores the typed node (`Stmt::Ast` / `Expr::Ast`)
as now. `Expr::Spelled` is built only by the `.AST` parse. The mode is one flag, as `keep_spelling` is today.

### 2.4 What is deleted

When the last family is moved, in this order, each its own PR with the three-path parity test of ADR-15334 §3.1:

1. `unspell` (nothing is spelled on the executing path) and the `Expr::Spelled` wrapper in executing builds;
2. the second `qualify_type_names` pass, folded into node construction (types are qualified from the scope when a
   type node is built; `qualify_type_names` remains for the keep-spelling path only if `.AST` needs the bare name,
   as it does for `Term::Name`);
3. `finalize_unit` and the `frontend::round_trip*` names: `round_trip_unit` becomes `finish_unit` (the passes that
   truly run on a whole unit: the whatever-priming scopes, scope diagnostics, `p6_frame_state`);
4. the thread-locals `CallerNames`, `CallerTraitTypes`, `with_caller_operators`, `DeclaredNames` and the
   `shadowed_terms` / `infix_func` installs, replaced by `NameScope` parameters.

`lower_converted`/`EVAL($ast)` keeps working throughout: it is the write direction and takes the same `NameScope`.

## 3. Consequences

- **Fewer disagreements, by construction.** Execution, `EVAL` of its source and `EVAL` of its `.AST` classify a name
  with one function over one value, in source order.
- **Cheaper.** One walk and one qualification are removed from every unit; the spelled wrapper is not built.
- **A real behaviour change to check:** `DeclaredNames::collect` classifies against the whole unit, so a name used
  *before* its declaration is today classified as declared. Source-order scope would make it a run-time lookup, as
  rakudo does for a type declared later in the unit but used earlier in a block. Each family's PR measures this
  against `raku` and the roast suite; a program that depends on the whole-unit behaviour is either a rakudo
  divergence (then the order wins) or needs the one hoisting rule rakudo has (subs, packages and `our`/`constant`
  with a `BEGIN`-time value), which `NameScope` models explicitly rather than by a pre-scan.
- **Risk — the largest refactor left in the frontend.** Every executing parse changes. Mitigation: the family-by-family
  order of ADR-15334 §2.2 is kept (one family per PR, each deleting its share of `finalize`), the mixed state is the
  one that exists today, and `scripts/dev gate --full` plus the frontend-cost script gate each PR.
- **Risk — slang activation.** A slang changes what the parser accepts mid-unit; its mode flags are parser state, not
  names, and stay where they are. ADR-0026's activation points must register into the `NameScope` when they declare
  operators or types.

## 4. Alternatives considered

- **Keep `finalize_unit`, only add `NameScope` as its argument.** Removes the thread-locals but keeps the second
  walk, the whole-unit pre-scan and the spelled wrapper. It is the cheapest first slice and is compatible with this
  ADR (slice 0 below), but alone it does not meet ADR-15334 §2.1.2.
- **Make lowering read the parser's tables (a global).** What ADR-15334 §2.3 rejected: hand-built trees and `EVAL`
  strings have no parser, so a node would lower differently by path.
- **Resolve names in a separate resolver pass over RakuAST (Rakudo's `resolve`).** `src/rakuast/resolve.rs` does
  this for the `$*CU` view. A pass over the finished tree is the post-hoc design this ADR replaces; the resolver
  stays a view for analysis tools, not the executing path's classifier.

## 5. Open questions (for the maintainer)

1. **Whole-unit versus source-order classification** (§3, first risk): accept the source-order rule as the target,
   with the measured divergences listed per family, or keep a pre-scan for the hoisted forms only?
2. **Slice 0 first?** Introduce `NameScope` as a parameter of `finalize_unit` and the `EVAL`/`Str.AST` entry points
   (no behaviour change), then move families, or go family-first with a thin `NameScope` per family?
3. **Where `NameScope` lives**: `src/scope.rs` (proposed) versus inside `src/rakuast/`, which `parser` would then
   depend on, against the layer ratchet.
