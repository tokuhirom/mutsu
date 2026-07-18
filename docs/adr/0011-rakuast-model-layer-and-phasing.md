# ADR-0011: RakuAST — a reflection/model layer over the internal AST, and its phasing

- **Status**: Accepted (2026-07-18). Phase 1 implemented (PR #4679); Phases 2–6 pending.
- **Context**: The user asked to implement RakuAST. RakuAST is Raku's user-facing AST
  representation (`RakuAST::*` classes, `Q|...|.AST`, `.DEPARSE`, `EVAL(ast)`, and the
  macro/`quasi` machinery built on top). It has essentially **zero roast payoff today**
  (a single skipped `isa-ok` in `roast/S32-str/format.t` is the only roast reference), so
  this is a deliberate *new capability* direction, not a BLOCKERS-driven slice. Because it
  is large and costly to reverse, the end-state design and phasing are fixed here before
  any implementation.

## Problem

Two things are true at once and they pull in opposite directions:

1. **RakuAST in Rakudo *is the compiler frontend*.** Rakudo's grammar produces RakuAST
   nodes directly and the backend lowers them to QAST/MoarVM. The RakuAST class hierarchy
   (~200+ classes: `RakuAST::Node` root, `RakuAST::Statement::*`, `RakuAST::Expression`,
   `RakuAST::Call::*`, `RakuAST::VarDeclaration::*`, literals, signatures, …) is the real
   IR, with typed attributes, `.visit-children`, `.IMPL-*` lowering hooks, and full
   construct-and-`EVAL` support that macros depend on.

2. **mutsu's frontend is not built that way and must not be rebuilt.** mutsu is
   `source → Expr/Stmt (internal AST, ast.rs) → bytecode`. Rebuilding the parser to emit
   RakuAST natively is a Rakudo-scale rewrite with no near-term payoff and enormous risk.

The design must deliver a genuine, extensible RakuAST that can grow to construction, EVAL,
and macros — **without** betting the interpreter's frontend on it.

## Decision

Treat RakuAST as a **reflection/model layer that is bidirectionally convertible with the
internal `Expr`/`Stmt` AST**, not as mutsu's compiler IR.

```
                       .AST  (read)
   Expr/Stmt  ───────────────────────────►  RakuAST node tree (Value::RakuAst)
  (internal IR)                              │  .gist / .raku / .DEPARSE / .^name / accessors
       ▲                                     │
       │        lower  (write, Phase 5)      ▼
       └───────────────────────────────  RakuAST node tree
   then existing compiler → bytecode → run (EVAL)
```

- **Read direction (`.AST`)**: parse source → internal AST → convert to a RakuAST node
  tree. This is what Phase 1 ships.
- **Write direction (`EVAL`/construction)**: build a RakuAST node tree (programmatically or
  from `.AST`), lower it back to internal `Expr`/`Stmt`, and feed the **existing** compiler.
  No second execution engine — consistent with "do NOT add new slow paths".

This mapping is intentionally **lossy in raku's favour, not ours**: mutsu's internal AST is
special-cased (e.g. `say 42` → `Stmt::Say`, `say(42)` → `Expr::Call`) and does no constant
folding (`1+2` stays a binop where raku folds to `IntLiteral(3)`). We map faithfully where
the internal AST preserves the distinction, **document every divergence from raku's exact
node choice**, and narrow the gaps over time. We never paper over a divergence with a
special-case hack (per the roast working agreements).

### Node representation

A single new `Value` variant carries every RakuAST node:

```rust
Value::RakuAst(Box<RakuAstNode>)

pub struct RakuAstNode {
    pub class: RakuAstClass,        // enum of all known node kinds (StatementList, IntLiteral, …)
    pub fields: Vec<RakuAstField>,  // ordered — drives gist/DEPARSE and named/positional accessors
}
pub struct RakuAstField {
    pub name: Option<&'static str>, // None = positional .new() arg; Some = named arg / accessor name
    pub value: Value,               // child RakuAst node, or a leaf (Int/Num/Str), or a List of nodes
    pub render: FieldRender,        // Plain | ParenList (parenthesised, trailing-comma list)
}
```

- `RakuAstClass` is a Rust enum, so the converter, the renderer, and the (future) lowerer
  all get **exhaustive `match` coverage** — adding a node kind is a compile error until every
  arm handles it. This is the mechanism that keeps the layer honest as it grows.
- **One metadata table** maps `RakuAstClass → { printed_name, constructor }` where
  `constructor ∈ { New, FromIdentifier, … }` (e.g. `RakuAST::Name` renders as
  `.from-identifier("x")`, not `.new(...)`). gist, DEPARSE, `.^name`, and (later) `.new` all
  read this one table — no duplicated per-class knowledge.
- Nodes are immutable trees with no back-edges, so `Box` (deep-clone on `Value::clone`) is
  the Phase-1 choice. **GC note (ADR-0001)**: `Value::RakuAst` is a container-kind variant
  (it holds child `Value`s). If profiling later shows clone churn, promote it to `Gc<…>` in
  the same type-filtered manner as the other container variants; scalar leaves inside it stay
  GC-free. Not a Phase-1 concern.

### Why a dedicated variant, not 200 registered classes up front

Registering the full `RakuAST::*` class hierarchy as real user-visible classes would give
`~~`, `.^name`, accessors, and `.new` "for free" via the class system — but 200+ classes is
a large static cost and most of that surface is unused until construction/EVAL exist. The
dedicated variant lets Phase 1 ship introspection with custom `.gist`/`.^name` immediately,
and Phase 3 adds a **thin type-object registry** (one metaobject handler routing `~~ /
.^name / accessor / .new` to the node's `class` + `fields`) **without changing the node
representation**. Representation is decided once, here; capability grows around it.

## Phasing

Each phase is independently shippable and testable; later phases do not require reworking
earlier ones.

- **Phase 1 — Introspection MVP (`.AST` + gist), read-only.** *(implement now)*
  - `Value::RakuAst`, `RakuAstNode`, `RakuAstClass` seeded with the literal + say-call
    cluster: `StatementList`, `Statement::Expression`, `IntLiteral`, `NumLiteral`,
    `RatLiteral`, `StrLiteral`, `QuotedString`, `Call::Name`, `Call::Name::WithoutParentheses`,
    `Name`, `ArgList`.
  - `ast_to_rakuast(&[Stmt]) -> RakuAstNode`.
  - `.AST` method on `Str`: `parse_source` → convert → `Value::RakuAst`.
  - Renderer for `.gist`/`.Str`/`.raku` matching raku exactly: 2-space indent per level,
    named-arg keys left-padded to the max key width in that `.new(` group, `ParenList`
    fields printed `(\n  elem,\n)` with a trailing comma. `.^name` → printed class name.
  - Tests: `t/rakuast-ast.t` pinning each construct against captured raku output.
- **Phase 2 — Read coverage expansion + `.DEPARSE`.** Grow the enum + converter cluster by
  cluster: variable use (`Var::Lexical`), `VarDeclaration::Simple` + `Initializer::Assign`,
  binary/unary ops (`ApplyInfix`/`ApplyPrefix`/`ApplyPostfix` + `Infixish`/`Prefixish`),
  method calls, blocks & pointy blocks, `if`/`unless`/`with`, loops, sub declarations,
  signatures & parameters. Add `.DEPARSE` (a second renderer, source form). Resolve the
  **constant-folding divergence** here (see Open questions).
- **Phase 3 — Type-object registry + introspection dispatch.** Register `RakuAST::*` type
  objects; route `~~ RakuAST::Node`, `.^name`, `.WHAT`, `.isa`, and attribute accessors
  (`$node.expression`, `$node.args`, `.statements`) to a central RakuAST metaobject over the
  node's `class`/`fields`. Accept `use experimental :rakuast` (no-op gate).
- **Phase 4 — Construction.** `.new` (and `.from-identifier`, …) on RakuAST type objects
  build `Value::RakuAst`, validating args against the per-class field schema.
- **Phase 5 — EVAL / compilation.** `lower(RakuAstNode) -> Vec<Stmt>/Expr`, then the
  **existing** compiler. `EVAL($rakuast)` and any code that yields a RakuAST tree runs
  through this. No new execution engine.
- **Phase 6 — Macros / `quasi`.** `macro`, `quasi { … }`, unquoting `{{{ … }}}`, AST
  splicing — built entirely on Phases 4+5. Most complex; may be deferred indefinitely.

### File layout

- `src/rakuast/mod.rs` — `RakuAstNode`, `RakuAstClass`, the class-metadata table.
- `src/rakuast/convert.rs` — internal AST → `RakuAstNode` (Phase 1+2).
- `src/rakuast/render.rs` — `.gist`/`.raku` and (Phase 2) `.DEPARSE`.
- `src/rakuast/lower.rs` — `RakuAstNode` → internal AST (Phase 5).
- `Value::RakuAst` variant in `src/value/mod.rs`; `.AST` dispatch in the `Str` method path.
- Each file stays under the 500-line cap; split per-cluster as it grows.

## Consequences

- **Positive**: introspection ships fast and standalone; the enum + single metadata table
  make expansion cheap and exhaustive-checked; construction/EVAL/macros reuse the existing
  compiler instead of a parallel engine; the frontend is never at risk.
- **Negative / accepted**: exact node-choice parity with raku is imperfect wherever the
  internal AST is lossy or unfolded — these are documented divergences, tracked and narrowed,
  not hidden. The dedicated-variant choice means Phase 3 must build a small metaobject shim
  rather than getting class semantics free.

## Open questions (resolve before the phase that needs them)

- **Constant folding (Phase 2).** raku folds constant sub-expressions (`1+2` → `IntLiteral(3)`)
  before producing RakuAST; mutsu does not. Options: (a) render mutsu's `ApplyInfix` faithfully
  and document the divergence, or (b) add a small const-fold pass in the converter for literal
  operands. Leaning (a) for fidelity to *mutsu's* AST, revisited with data.
- **`.AST` on `Code`/blocks (Phase 2+).** Phase 1 is `Str`-only; `.AST` on a `Block`/`Routine`
  needs the internal AST retained on the code object.
- **`unless`/`until` fold to `if !`/`while !` (Phase 2 slice 4).** mutsu's parser desugars
  `unless X { }` to `if !X { }` and `until X { }` to `while !X { }` at parse time — there is no
  distinct `Unless`/`Until` node in the internal AST. So `unless X.AST` renders
  `RakuAST::Statement::If` with a negated `ApplyPrefix(!)` condition (and `until` renders
  `Statement::Loop::While` likewise), whereas raku keeps `RakuAST::Statement::Unless` /
  `Loop::Until` with the bare condition. This is faithful to *mutsu's* AST (the desugaring is
  lossless-but-collapsing: `if !X` and `unless X` are the same node), so reconstituting the
  surface keyword would be a guess — deliberately not done. Narrows only if the parser grows
  distinct `unless`/`until` statements. `elsif` chains are handled (slice 5): mutsu nests each
  `elsif` as a single `if` inside the else-branch, and the converter flattens that chain into
  raku's flat `elsifs` list (`Statement::Elsif`), with any trailing block as the final `else`.
  C-style and `repeat` loops are handled (slice 8): `loop (init; cond; step) { }` →
  `Statement::Loop(setup, condition, increment, body)` (each clause present only when written; the
  `setup` renders its `VarDeclaration`/expression node **unwrapped**, not inside a
  `Statement::Expression` — and because the loop-init parse path does not set `__has_initializer`,
  the initializer is detected from a non-`Nil` `expr`); `repeat { } while X` →
  `Statement::Loop::RepeatWhile(body, condition)`, with `repeat ... until X` folding to
  `RepeatWhile` + negated condition (same collapse as `until`/`while`). Labelled loops and topic
  binding (`if EXPR -> $v` / `elsif EXPR -> $v`) remain the coverage boundary (explicit
  `RuntimeError`), deferred to a later slice. Implicit-topic `for`
  is handled (slice 6): `for SRC { ... }` (no explicit signature) → `Statement::For(mode =>
  "serial", source, body)` where the body is a topic-taking `Block` marked `implicit-topic => True`
  / `required-topic => 1`. `with`/`without` are NOT mappable — mutsu desugars them at parse time
  into a temp-var + `.defined` check + `if` (`__with_tmp_0`), so there is no `Statement::With` node
  to recover (same collapse as `unless`/`until`); deferred. Explicit-signature `for` (`for @a ->
  $x`), hyper/race/lazy modes, `<->` rw blocks, and labels also remain the boundary. Note the
  topic-call form `.method` (i.e. `$_.method` written bare) is desugared by mutsu to `$_.method`
  (`ApplyPostfix` on `$_`), where raku keeps a distinct `Term::TopicCall` — a pre-existing
  method-call representation divergence, unrelated to `for`.
- **Named sub declarations (Phase 2 slice 7).** `sub NAME (params) { body }` (`Stmt::SubDecl`) →
  `Statement::Expression(Sub(name => Name, [signature => Signature], body => Blockoid))`. A
  parameter-less sub (and an empty explicit `()` signature) omits the `signature` field. Sub/method
  signature parameters carry an implicit `type => Type::Setting(Name.from-identifier("Any"))` that
  pointy-block parameters do not — the `signature`/`parameter`/`simple_parameter` helpers thread a
  `type_setting` flag to add it. Boundary (explicit `RuntimeError`): traits, `multi`, `is export`,
  return types, operator subs (associativity/precedence), alternate signatures, and anonymous
  `sub { }` (deferred). Parameters reuse the slice-3 plain-positional boundary (typed/named/slurpy/
  `where`/… still error).
- **Comma lists and `:=` binding (Phase 2 slice 9).** A bare comma list `1, 2, 3`
  (`Expr::ArrayLiteral`) → `ApplyListInfix(infix => Infix(","), operands => (...))`. A `:=` bind
  (`Stmt::Assign { op: Bind }`) → `ApplyInfix(left, infix => Infix(":="), right)` — a plain `Infix`,
  unlike `=`'s special `Assignment` node. Divergences: a **parenthesised** list `(1, 2)` is
  `Circumfix::Parentheses(SemiList(...))` in raku, but mutsu drops the parens at parse time, so it
  renders as a bare `ApplyListInfix` (deferred). And a **compound** assignment `$x += 3` is
  desugared by mutsu to `$x = $x + 3` (`op` stays `Assign`), so it renders as a plain `=` over a
  binop rather than raku's `MetaInfix::Assign(Infix("+"))` (deferred, same collapse family as
  `unless`/`until`).
- **Scoped/typed declarations (Phase 2 slice 10).** `my`/`our`/`state` with an optional simple
  type: `VarDeclaration::Simple` gains a `scope` leaf (`"our"`/`"state"`; `my` is the default and
  omitted) and a `type => Type::Simple(Name)` field, in raku's field order (scope, type, sigil,
  desigilname, initializer). Only plain (possibly `::`-qualified) type identifiers map to
  `Type::Simple`; parameterised (`Array[Int]`), definite (`Int:D`), and coercion (`Str()`) types,
  dynamic (`$*x`) declarations, `where` constraints, and real `is`/`does` traits are the coverage
  boundary (explicit `RuntimeError`).
- **Single-param pointy sigil loss (Phase 2 slice 3).** mutsu parses a single-parameter pointy
  block to `Expr::Lambda { param: String }` with the sigil stripped, and does not preserve `@`/`%`
  for a single non-scalar param (`-> @a` becomes `param: "a"`). The converter therefore assumes `$`
  for `Lambda`, so `-> @a { }`/`-> %h { }` render `ParameterTarget::Var(name => "$a")` instead of
  raku's `"@a"`. Multi-parameter pointy blocks (`Expr::AnonSubParams`) keep the sigil and render
  correctly. Documented divergence; narrows if `Lambda` grows a sigil field.
- **GC promotion of `Value::RakuAst`** — deferred until profiling shows clone churn (see note
  above).
- **`.Str` / string context (Phase 1 simplification).** raku's RakuAST `.Str` (and bare string
  context) yields the default object form `RakuAST::X<addr>` and warns; only `.gist`/`say`/`.raku`
  render the constructor form. Phase 1 routes mutsu's single stringification path
  (`to_string_value`) to the gist, so `.Str`/`~` also produce the constructor form. This is a
  harmless divergence (no existing code stringifies a RakuAST node, and the raku form embeds a
  non-deterministic address); revisit if/when `.gist` and `.Str` need to diverge.

## References

- `raku-doc/doc/Type/RakuAST.rakudoc`, `raku-doc/doc/Type/RakuAST/Doc.rakudoc`.
- Captured raku `.AST` output for the Phase-1 cluster is pinned in `t/rakuast-ast.t`.
- ADR-0001 (GC type-filter) for the `Value::RakuAst` promotion note.
