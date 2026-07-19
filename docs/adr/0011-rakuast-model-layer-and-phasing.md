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
  - **Slice 1 (accessors) — done.** `node_accessor(node, method)` (dispatched in the 0-arg method
    path) returns a named field's value as a mutsu `Value` (child node, leaf `Int`/`Str`, or a
    `List` for list-valued fields), and `.statements` returns a `StatementList`'s positional
    children as a `List`. This makes the tree walkable (`.AST.statements[0].condition.^name`), not
    just renderable. `.gist`/`.raku`/`.^name` are unaffected (not field names). Tests in
    `t/rakuast-accessors.t`.
  - **Slice 2 (smartmatch hierarchy) — done.** `$node ~~ RakuAST::TypeName` and `.isa(...)` now
    honour the RakuAST hierarchy: `isa_check` returns true for the base `RakuAST::Node` (any node)
    and for any `::`-namespace ancestor of the node's printed class (`Statement::If isa
    RakuAST::Statement`); the `::` boundary avoids a false `StatementList isa Statement`. The
    smartmatch path was already routing exact-class matches through `isa_check`; `type_matches_value`
    now delegates `RakuAST::*` constraints to `isa_check` instead of the static name table. Tests in
    `t/rakuast-smartmatch.t`.
  - **Slice 3 (positional-leaf accessors) — done.** `IntLiteral.value` / `RatLiteral.value` /
    `StrLiteral.value` return the literal payload, and `Var::Lexical.name` returns the sigil'd
    variable string — the single positional field exposed under a class-specific accessor name. The
    named-field lookup runs first, so `Call::Name.name` (a named `Name`-node field) is not shadowed.
    Tests in `t/rakuast-leaf-accessors.t`.
  - **Slice 4 (semantic Expression/Term hierarchy) — done.** `IntLiteral isa RakuAST::Expression`,
    `IntLiteral isa RakuAST::Term`, `ApplyInfix isa RakuAST::Expression` (but not `Term`), etc. —
    the ancestor names that don't appear in the printed class name. A `RakuAstClass::
    semantic_ancestors` table (verified against Rakudo: literals/variables/`Term::Reduce`/`Sub`/
    blocks/`Call::Name` are Terms-and-Expressions; `Apply*`/`Ternary` are Expressions-only; `Name`/
    `ArgList`/`Signature`/statements/types are neither) is consulted by `isa_check`. Conservative:
    only verified classes are listed, so an unlisted expression node is a missed match, never a
    false positive. Tests in `t/rakuast-semantic-hierarchy.t`. Still pending: registering `RakuAST::*`
    as first-class type objects (they currently resolve as bare type names).
- **Phase 4 — Construction.** `.new` (and `.from-identifier`, …) on RakuAST type objects
  build `Value::RakuAst`, validating args against the per-class field schema.
  - **Slice 1 (literals) — done.** `RakuAST::IntLiteral.new(42)` / `RatLiteral.new(3.5)` /
    `StrLiteral.new("hi")` build a real node — renderable (`.gist` round-trips) and queryable
    (`.value`, `~~`). A `rakuast::construct(class_name, method, args)` builds the node; it is
    dispatched from the `.new`-on-a-`Package` handler (`methods_object_dispatch_new`) for any
    `RakuAST::*` type-object bareword. Tests in `t/rakuast-construct.t`.
  - **Slice 2 (`Name.from-identifier`) — done.** `RakuAST::Name.from-identifier("x")` builds a Name
    node (its gist round-trips). Same single-positional builder as the literals. `.new` routes
    through `methods_object_dispatch_new`, but a non-`new` constructor like `from-identifier` reaches
    the terminal method-not-found fallback in `methods_instance_ops` instead, so the RakuAST
    construct hook is placed there too. Tests in `t/rakuast-construct-name.t`.
  - **Slice 3 (multi-field) — done.** Bare operator nodes (`Infix.new("+")`, `Prefix.new(...)`) use
    the single-positional builder; named-field constructors (`Statement::Expression.new(expression =>
    …)`, `ApplyInfix.new(left => …, infix => …, right => …)`) build fields from the `key => value`
    args in a per-class schema order. Named args reach `construct` as `Pair`/`ValuePair` values;
    `named_arg` finds them, and a missing one is a clear error. Constructed nodes nest, render (gist),
    and are queryable (`.left.value`, `~~ RakuAST::Expression`). Tests in
    `t/rakuast-construct-multi.t`.
  - **Slice 4 (more operators) — done.** `Var::Lexical.new("$x")` (positional), `ApplyPrefix.new(
    prefix => …, operand => …)`, `ApplyPostfix.new(operand => …, postfix => …)`, and `Postfix.new(
    operator => …)` (whose `operator` is a *named* string field). Tests in
    `t/rakuast-construct-more.t`. Next: `StatementList` (its children are added via the `.add-statement`
    *mutator*, which needs a mutable-node path), block/sub/declaration constructors — then Phase 5
    (EVAL) can lower a fully-constructed tree.
- **Phase 5 — EVAL / compilation.** `lower(RakuAstNode) -> Vec<Stmt>/Expr`, then the
  **existing** compiler. `EVAL($rakuast)` and any code that yields a RakuAST tree runs
  through this. No new execution engine.
  - **Slice 1 (literals) — done.** `src/rakuast/lower.rs` lowers a RakuAST node back to internal
    `Stmt`/`Expr`; `builtin_eval` now recognises a `Value::RakuAst` first argument, lowers it, and
    runs it via `eval_block_value` (the existing evaluator). `EVAL(RakuAST::IntLiteral.new(42))` →
    `42`, and `EVAL(Q[42].AST)` round-trips source → node tree → value. Slice 1 lowers the literal
    cluster (`IntLiteral`/`RatLiteral`/`StrLiteral`, single-segment `QuotedString`) plus the
    `StatementList` / `Statement::Expression` wrappers; anything else is an explicit `RuntimeError`.
    Tests in `t/rakuast-eval.t`.
  - **Slice 2 (infix/prefix) — done.** `ApplyInfix` lowers to `Expr::Binary` and `ApplyPrefix` to
    `Expr::Unary`; the operator string of the `Infix`/`Prefix` child maps back to a `TokenKind` via a
    new `op_name_to_token_kind` (the reverse of `token_kind_to_op_name`, covering the common
    arithmetic/comparison infixes). `EVAL(Q[3 * 4 + 1].AST)` → `13`. Operators outside the reverse map
    are an explicit `RuntimeError`. Tests in `t/rakuast-eval-infix.t`.
  - **Slice 3 (variables & declarations) — done.** `Var::Lexical("$x")` lowers to the sigil-specific
    variable expression (`Expr::Var`/`ArrayVar`/`HashVar`/`CodeVar`), and a plain `my $x = EXPR`
    (`VarDeclaration::Simple`) lowers to `Stmt::VarDecl` (declaration-inside-`Statement::Expression`
    becomes its own statement, not a `Stmt::Expr`). `EVAL(Q[my $x = 5; $x * 2].AST)` → `10` — a
    multi-statement program round-trips. Scoped/typed/attribute declarations and non-scalar
    initializers (comma lists) stay the boundary. Tests in `t/rakuast-eval-var.t`. Next: method
    calls, `if`/loops, sub declarations — the inverse of the Phase-2 converter, grown cluster by
    cluster.
  - **Slice 4 (calls) — done.** The listop I/O calls (`Call::Name`/`::WithoutParentheses` with name
    `say`/`put`/`print`/`note`) lower to their statement forms (`Stmt::Say` etc.), and a postfix
    method call (`ApplyPostfix` with a `Call::Method` postfix) lowers to `Expr::MethodCall`.
    `EVAL(Q[my $x = -5; $x.abs].AST)` → `5`; chained calls and `say` as a side-effecting statement
    work. (The *return value* of a bare trailing `say` differs between mutsu and raku — a pre-existing
    mutsu EVAL behaviour also seen with a string EVAL, unrelated to RakuAST.) Tests in
    `t/rakuast-eval-call.t`. Next: `if`/loops, sub declarations.
  - **Slice 5 (control flow + assignment) — done.** `Statement::If` lowers to `Stmt::If`,
    `Statement::Loop::While` to `Stmt::While`, and an `ApplyInfix` whose infix is an `Assignment`
    lowers to `Stmt::Assign` (its block bodies via a `lower_block` `Block → Blockoid → StatementList`
    helper). `EVAL(Q[my $n = 5; my $f = 1; while $n > 1 { $f = $f * $n; $n = $n - 1 }; $f].AST)` → `120`.
    `elsif` chains stay the boundary. Tests in `t/rakuast-eval-control.t`. Next: `for`/`given`, sub
    declarations.
  - **Slice 6 (`for` + `elsif` chains + comma lists) — done.** `Statement::For` (single-parameter
    pointy block) lowers to `Stmt::For`; each `Statement::Elsif` in an `if`'s `elsifs` list folds into
    a nested `Stmt::If` in the enclosing `else`; and an `ApplyListInfix` with a `,` infix lowers to
    `Expr::ArrayLiteral`. `EVAL(Q[my $p = 1; for (2, 3, 4) -> $n { $p = $p * $n }; $p].AST)` → `24`.
    The bare `for @x { … }` (`$_`) form, multi-parameter blocks, and non-comma list infixes (`Z`/`X`)
    stay the boundary. Tests in `t/rakuast-eval-for-elsif.t`. Next: sub declarations, `given`/`when`.
  - **Slice 7 (`sub` declarations + named calls) — done.** `RakuAST::Sub` (with bare positional
    scalar parameters) lowers to `Stmt::SubDecl`, and a `Call::Name` lowers to `Expr::Call`, so a
    defined routine can be invoked — including recursively. `EVAL(Q[sub fact($n) { if $n < 2 { 1 } else
    { $n * fact($n - 1) } }; fact(5)].AST)` → `120`. Typed/named/slurpy/defaulted parameters and
    anonymous subs in expression position stay the boundary. Tests in `t/rakuast-eval-sub.t`. Next:
    `given`/`when`, the bare `for @x { … }` (`$_`) form.
  - **Slice 8 (`given`/`when`/`default`) — done.** `Statement::Given`/`When`/`Default` lower to
    `Stmt::Given`/`When`/`Default`, so a topicalizer block runs and yields the matched clause's value —
    `EVAL(Q[my $x = 2; given $x { when 1 { 10 }; when 2 { 20 }; default { 30 } }].AST)` → `20`. This also
    fixed a latent `eval_block_value` bug: `given`/`when`/`default` leave their block value on the value
    stack even in sink position (the tail-statement arms rely on that leaked value being the block
    result), so a *non-last* one shadowed the block's real tail value; `compile_unit` now pops it. Tests
    in `t/rakuast-eval-given.t`. Next: the bare `for @x { … }` (`$_`) form, control flow (`return`/
    `last`/`next`).
  - **Slice 9 (bare `for @x { … }` / `$_`) — done.** A `Statement::For` whose body is a plain `Block`
    (not a `PointyBlock`) lowers to a `Stmt::For` with no named parameter, so the loop body sees the
    iterand as `$_`. `EVAL(Q[my $t = 0; for 1..4 { $t = $t + $_ }; $t].AST)` → `10`. Tests in
    `t/rakuast-eval-bare-for.t`. Next: control flow (`return`/`last`/`next`), multi/typed parameters.
  - **Slice 10 (`return`/`last`/`next`) — done, both directions.** raku models these control-flow
    statements as bare calls (`Call::Name` in WithoutParentheses form). The read side (`.AST`) now emits
    that form — `Stmt::Return`/`Last`/`Next` → a `control_call` (the `args` field omitted when empty,
    matching raku's bare-call gist) — and the write side (`EVAL`) lowers a `return`/`last`/`next` call
    back to the internal control-flow statement. `EVAL(Q[sub f($x) { if $x > 0 { return 5 }; -1 }; f(3)].AST)`
    → `5`; `last`/`next` control loops. Labelled `last LABEL`/`next LABEL` stay the boundary. Tests in
    `t/rakuast-eval-return.t`. Next: multi/typed/named parameters, `unless`/`until`.
  - **Slice 11 (`unless`/`until` + prefix `!`/`?`) — done.** mutsu desugars `unless X` to `if !X` and
    `until X` to `while !X`, so lowering these only needed the prefix `!` (and `?`) operator added to the
    reverse operator map (`op_name_to_token_kind`, used solely by the lowerer). `EVAL(Q[my $i = 0; until
    $i >= 3 { $i = $i + 1 }; $i].AST)` → `3`; `EVAL(Q[my $x = 0; !$x].AST)` → `True`. Tests in
    `t/rakuast-eval-unless-until.t`. Next: multi/typed/named parameters, `repeat`/`while`.
  - **Slice 12 (default sub parameters) — done.** A `Parameter` carrying a `default` expression now
    lowers to an optional positional `ParamDef` (`required = false`, `default = Some(…)`) instead of
    being deferred, so a defaulted parameter uses its fallback when the argument is omitted.
    `EVAL(Q[sub f($x, $y = 10) { $x + $y }; f(5)].AST)` → `15`; a default may reference an earlier
    parameter (`$y = $x * 2`). Typed (`Int $x`) and named (`:$x`) parameters still lack read-side
    (`.AST`) support and stay the boundary. Tests in `t/rakuast-eval-default-param.t`. Next: `repeat`/
    `while`, typed/named parameters (both directions).
  - **Slice 13 (`repeat`/`while` and `repeat`/`until`) — done.** `Statement::Loop::RepeatWhile` lowers
    to a `Stmt::Loop` with `repeat => true`, which runs the body once before testing the condition.
    `repeat … until C` desugars to `repeat … while !C` (the prefix `!` from slice 11), so both forms
    share the write path. `EVAL(Q[my $i = 0; repeat { $i = $i + 1 } until $i >= 5; $i].AST)` → `5`, and
    the body always runs at least once. Tests in `t/rakuast-eval-repeat.t`. Next: `Bool` literals
    (read side), typed/named parameters (both directions), C-style `loop`.
  - **Slice 14 (`True`/`False` literals) — done, both directions.** A new `Term::Enum` node class
    models a boolean literal (`RakuAST::Term::Enum.from-identifier('True')` — the enum identifier is
    single-quoted in raku's gist, unlike `Name.from-identifier("…")`). The read side (`.AST`) converts
    a `Bool` literal to that node and the write side (`EVAL`) lowers `True`/`False` back to the Bool
    value. `EVAL(Q[my $i = 0; while True { $i = $i + 1; last if $i >= 3 }; $i].AST)` → `3`. Other enum
    identifiers stay the boundary. Tests in `t/rakuast-eval-bool.t`. Next: ternary lowering (`?? !!`),
    typed/named parameters, C-style `loop`.
  - **Slice 15 (ternary `?? !!`) — done.** `RakuAST::Ternary` lowers to `Expr::Ternary`, so a
    conditional expression evaluates its chosen branch. `EVAL(Q[sub even($n) { $n %% 2 ?? True !! False
    }; even(4)].AST)` → `True`; nested (right-associative) ternaries work. Tests in
    `t/rakuast-eval-ternary.t`. Next: typed/named parameters, C-style `loop`.
  - **Slice 16 (interpolated strings) — done, both directions.** An interpolated string `"a $x b"` is a
    `QuotedString` with one segment per part (a literal run is a `StrLiteral`, an interpolated term
    keeps its own node). The read side (`.AST`) converts `Expr::StringInterpolation` to that node (a
    literal segment stays a bare `StrLiteral`, not a nested `QuotedString`) and the write side (`EVAL`)
    lowers a multi-segment `QuotedString` back to a `StringInterpolation`. `EVAL(Q[my $x = 5;
    "val=$x"].AST)` → `val=5`. Code-block (`{…}`) interpolation stays the boundary. Tests in
    `t/rakuast-eval-interp.t`. Next: typed/named parameters, C-style `loop`.
  - **Slice 17 (typed positional parameters) — done, both directions.** A `Parameter` with an explicit
    type (`Int $x`) now carries `type => Type::Simple(Name)` (replacing the implicit
    `Type::Setting(Any)`) on the read side, and the write side extracts a `Type::Simple` name into the
    `ParamDef`'s `type_constraint` (the implicit `Type::Setting` is ignored; definite/coercion/
    parameterised type forms defer). `EVAL(Q[sub f(Int $x) { $x * 2 }; f(5)].AST)` → `10`; a type
    mismatch throws, and a typed parameter can also carry a default. Tests in
    `t/rakuast-eval-typed-param.t`. Next: named/slurpy parameters, more infix operators (`x`/`eq`/…),
    C-style `loop`.
  - **Slice 18 (named infix operators) — done.** The read side already renders `$a x $b` as
    `Infix.new("x")`; the write side now maps `~~` to `SmartMatch` and any other operator name to
    mutsu's generic `Ident` token (the inverse of `token_kind_to_op_name`'s `Ident(name) => name`), so
    `x`/`xx`/`eq`/`ne`/`lt`/`gt`/`le`/`ge`/`cmp`/`leg`/`div`/`mod`/… all lower at once.
    `EVAL(Q["ab" x 3].AST)` → `ababab`; `EVAL(Q[my $x = 5; $x ~~ 5].AST)` → `True`. Bareword type terms
    (`Int` as a smartmatch RHS) stay the boundary. Tests in `t/rakuast-eval-named-infix.t`. Next:
    bareword type terms, named/slurpy parameters, C-style `loop`.
  - **Slice 19 (assignment expressions) — done, both directions.** An `AssignExpr` in value position
    (e.g. the inner `$b = 5` of a chained `$a = $b = 5`) converts to the same `ApplyInfix(Assignment)`
    as a statement assignment (reusing `assignment_infix`), and the write side lowers an
    `ApplyInfix(Assignment)` in expression position back to an `AssignExpr` (a shared `lower_assign_parts`
    backs both the statement and expression forms). `EVAL(Q[my $a; my $b; $a = $b = 5; $a + $b].AST)` →
    `10`. Explicitly-parenthesised `($x = 5)` (a `Circumfix::Parentheses`) and `:=` binding stay the
    boundary. Tests in `t/rakuast-eval-assign-expr.t`. Next: C-style `loop`, bareword type terms.
  - **Slice 20 (C-style `loop`) — done.** `Statement::Loop` (with optional `setup`/`condition`/
    `increment`) lowers to a `Stmt::Loop`; the read side already works via the slice-19 assignment
    expression (the `$i = $i + 1` increment). `EVAL(Q[my $s = 0; loop (my $i = 0; $i < 5; $i = $i + 1) {
    $s = $s + $i }; $s].AST)` → `10`; a bare `loop { … }` broken by `last`, and `next`/`last` inside,
    all work. Tests in `t/rakuast-eval-cstyle-loop.t`. Next: bareword type terms, named/slurpy
    parameters, parenthesisation.
  - **Slice 21 (parenthesised expressions) — done, both directions.** A new `Circumfix::Parentheses`
    node class models a standalone `(EXPR)` (wrapping a single-statement `SemiList`). The read side
    (`.AST`) converts `Expr::Grouped` to it and the write side (`EVAL`) unwraps it to the inner
    expression. This closes the slice-19 boundary: `EVAL(Q/my $x = 0; my $y = ($x = 5); $x + $y/.AST)` →
    `10`. Multi-statement parenthesised lists stay the boundary. Tests in `t/rakuast-eval-paren.t`.
    Next: bareword type terms, named/slurpy parameters, array/hash literals.
  - **Slice 22 (named parameters) — done, both directions.** A named parameter `:$x` renders as a
    `Parameter` with a `names` list (and no `optional`, since named params are optional by default);
    the write side sets the `ParamDef`'s `named` flag. The `x => 5` call argument (a `FatArrow` infix,
    which renders with its Debug name) also lowers now (`op_name_to_token_kind` maps `"FatArrow"`).
    `EVAL(Q[sub f(:$x) { $x * 2 }; f(x => 5)].AST)` → `10`; the `:x(7)` colonpair form and a
    positional+named mix work, and an omitted named param is undefined. Typed/defaulted named params
    stay the boundary. Tests in `t/rakuast-eval-named-param.t`. Next: bareword type terms, slurpy
    parameters, array/hash literals.
  - **Slice 23 (slurpy parameters) — done, both directions.** A slurpy parameter `*@a` (or `**@a`)
    renders as a `Parameter` with a `slurpy => RakuAST::Parameter::Slurpy::Flattened` (resp.
    `Unflattened`) marker — a new pair of node classes that render as a *bare class name* (no
    constructor call, via `renders_bare()`). The write side sets the `ParamDef`'s `slurpy`/
    `double_slurpy` flag. `EVAL(Q[sub f(*@a) { @a.elems }; f(1, 2, 3)].AST)` → `3`; a leading
    positional plus a slurpy, `.sum` over the slurpy, and an empty slurpy all work. Typed slurpies and
    `+@a` (onearg) stay the boundary. Tests in `t/rakuast-eval-slurpy.t`. Next: bareword type terms,
    array/hash literals.
  - **Slice 24 (array-composer literals) — done, both directions.** A `[1, 2, 3]` literal renders as a
    new `Circumfix::ArrayComposer` node wrapping a single-statement `SemiList` (a comma list — the
    conversion reuses a shared `comma_list_node`). The read side converts `Expr::BracketArray` to it and
    the write side unwraps it to an array literal (`Expr::BracketArray`). `EVAL(Q{[1, 2, 3].elems}.AST)`
    → `3`; `.sum`/`.sort.join` over the literal and a single-element `[5]` work. This also fixes the
    previous `[…].method` convert error. Positional subscripts (`@a[1]` — an `ApplyPostfix` with a
    `Postcircumfix::ArrayIndex`) stay the boundary. Tests in `t/rakuast-eval-array-lit.t`. Next:
    positional subscripts, hash literals, bareword type terms.
  - **Slice 25 (positional subscripts) — done.** An `ApplyPostfix` with a `Postcircumfix::ArrayIndex`
    postfix (`@a[EXPR]`) lowers to `Expr::Index` (the index is the `SemiList`'s single
    `Statement::Expression`); the read side already emits it. `EVAL(Q{my @a = 10, 20, 30; @a[1]}.AST)` →
    `20`; a computed index, subscripting an array literal (`[10, 20, 30][2]`), and two subscripts in an
    expression work. Associative subscripts (`%h{…}`) and slices (`@a[1, 2]`) stay the boundary. Tests
    in `t/rakuast-eval-subscript.t`. Next: hash literals, bareword type terms, code-block interpolation.
  - **Slice 26 (bareword type terms) — done, both directions.** A bare type name used as a value
    (`Int`, `Str`) converts to a `Type::Simple` on the read side — but only when it is a *known* type
    (`is_known_type_constraint`), so a non-type bareword (`foo`) stays the boundary — and the write side
    lowers a `Type::Simple` in expression position back to a bareword term (which mutsu evaluates to the
    type object). This closes the slice-18 `~~ Int` boundary: `EVAL(Q{5 ~~ Int}.AST)` → `True`,
    `EVAL(Q{"x" ~~ Int}.AST)` → `False`; `Int.^name` → `"Int"`, and a type object can be stored in a
    variable. User-defined types stay the boundary. Tests in `t/rakuast-eval-typeterm.t`. Next: hash
    literals (raku models `{a => 1}` as a `Block`), code-block interpolation.
  - **Slice 27 (the `*` whatever term) — done, both directions.** A new `Term::Whatever` node class
    models a bare `*` (it renders as a bare `.new` — no parens — via `empty_parens_omitted`). The read
    side converts `Expr::Whatever` to it and the write side lowers it back. `EVAL(Q{(1..*).head(3).join(",")}.AST)`
    → `"1,2,3"`; `*` closes an infinite range and a bare `*` is a `Whatever` value. `WhateverCode`
    (`* + 1`, `*-1`), which mutsu desugars to a closure, stays the boundary. Tests in
    `t/rakuast-eval-whatever.t`. Next: hash literals, code-block interpolation, WhateverCode.
  - **Slice 28 (fat-arrow pairs) — done, both directions.** A new `FatArrow` node class models a pair
    `a => 1` (`key` leaf + `value` node). The read side converts a `Expr::PositionalPair` over a
    `FatArrow` binop with a string-literal key, and the write side lowers a `FatArrow` back to that
    positional pair. `EVAL(Q{(a => 5).value}.AST)` → `5`; `.key`/`.kv` and an expression value work.
    (A *bare* `a => 1` statement renders as an `ApplyInfix` in mutsu — matching the call-argument form —
    so the pin checks round-tripped values rather than the gist.) A non-string-literal key stays the
    boundary. Tests in `t/rakuast-eval-pair.t`. Next: hash literals, code-block interpolation,
    WhateverCode.
  - **Slice 29 (hash literals — read only) — done.** raku models `{a => 1}` as a `Block` whose body is
    a `FatArrow` pair (or a comma list of `FatArrow`s), not a dedicated hash node. The read side now
    converts `Expr::Hash` to that `Block` (via `hash_literal_node`), so the `.AST` gist matches Rakudo.
    This is **read-only**: EVAL of the produced `Block` yields a block/Callable in raku itself (`{a =>
    1}.AST` round-trips to a `Block`, and `.elems` on it is `1`, not the hash's size) — the
    block-vs-hash distinction is a parse-time decision the RakuAST `Block` node does not carry — so the
    write direction is deliberately out of scope. Value-less keys (`{:a}`) stay the boundary. Tests in
    `t/rakuast-hash-literal.t`. Next: code-block interpolation, WhateverCode, associative subscripts.
  - **Slice 30 (the `do` statement prefix) — done, both directions.** A new `StatementPrefix::Do` node
    class models `do { … }`. The read side converts `Expr::DoBlock` to it (wrapping the block via
    `block_node`) and the write side lowers it back to an `Expr::DoBlock` over the lowered block body.
    `EVAL(Q{do { 1 + 2 }}.AST)` → `3`; a multi-statement do block, a do block over a variable, and a do
    block composed inline in an expression all work. A labelled `do` stays the boundary. Tests in
    `t/rakuast-eval-do.t`.
  - **Slice 31 (the `try` statement prefix) — done, both directions.** A new `StatementPrefix::Try`
    node class models `try { … }`. The read side converts `Expr::Try` (without a `CATCH` block) to it
    and the write side lowers it back to an `Expr::Try`. `EVAL(Q{try { 1 + 2 }}.AST)` → `3`, and a
    *failing* try yields an undefined value (`EVAL(Q{try { (1/0).Int }}.AST).defined` → `False`) — the
    exception is trapped. A `try` with a `CATCH` block (and a bare `die` statement inside) stay the
    boundary. Tests in `t/rakuast-eval-try.t`. Next: `die` (a control call), `gather`/`take`,
    code-block interpolation.
  - **Slice 32 (`die`/`fail`) — done, both directions.** Like `return`/`last`/`next`, raku models
    `die`/`fail` as bare calls. `Stmt::Die`/`Stmt::Fail` convert to a `Call::Name` (via `control_call`),
    and the write side lowers a `die`/`fail` call back to the control-flow statement. This unblocks a
    `die` inside a `try` body: `EVAL(Q{try { die "boom"; 1 }}.AST).defined` → `False`, and the message
    reaches `$!`. A conditional `die` is caught by a surrounding `try`. Tests in `t/rakuast-eval-die.t`.
    Next: `gather`/`take`, CATCH blocks, code-block interpolation.
  - **Slice 33 (`gather`/`take`) — done, both directions.** `gather { … }` is a new
    `StatementPrefix::Gather` node (like `do`/`try`), and `take EXPR` is a bare call (like `die`).
    `Expr::Gather` and `Stmt::Take` convert (`take-rw` deferred) and lower back.
    `EVAL(Q{gather { take 1; take 2; take 3 }.elems}.AST)` → `3`; `take` inside a loop gathers each
    iteration, and a conditional `take` gathers a subset. Tests in `t/rakuast-eval-gather.t`. Next:
    CATCH blocks, more statement prefixes (`quietly`/`lazy`/`eager`), code-block interpolation.
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
  `RuntimeError`), deferred to a later slice. Loop labels are handled (slice 17): a
  `LABEL: while/for/loop` prepends a `labels => (Label(name => "..."),)` field (raku renders labels
  first). mutsu stores the label inline on `while`/`for`/bare-`loop`; a labelled `repeat` is wrapped
  in a separate `Stmt::Label { name, stmt }` node (slice 28) — the converter converts the inner
  statement and prepends its `labels` field. A labelled C-style `loop` does not parse in mutsu yet,
  so it remains the boundary. Implicit-topic `for`
  is handled (slice 6): `for SRC { ... }` (no explicit signature) → `Statement::For(mode =>
  "serial", source, body)` where the body is a topic-taking `Block` marked `implicit-topic => True`
  / `required-topic => 1`. `with`/`without` are NOT mappable — mutsu desugars them at parse time
  into a temp-var + `.defined` check + `if` (`__with_tmp_0`), so there is no `Statement::With` node
  to recover (same collapse as `unless`/`until`); deferred. Explicit-signature `for` (`for @a ->
  $x`) is handled (slice 12): the body becomes a `PointyBlock` carrying the signature (reusing the
  slice-3 pointy-block logic, no `Type::Setting`) instead of a topic-marked `Block`; a single param
  lives in mutsu's `For.param_def`, multiple in `For.params_def`. Hyper/race/lazy modes, `<->` rw
  blocks, and labels remain the boundary. Note the
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
- **Anonymous parameter-less `sub { }` (Phase 2 slice 11).** `sub { ... }` with no signature
  (`Expr::AnonSub { is_block: false }`) → `RakuAST::Sub(body => Blockoid)` with no `name` field.
  Only the no-parameter form is handled: `sub ($x) { }` parses to `Expr::AnonSubParams`, which mutsu
  cannot distinguish from a multi-param pointy block (`-> $a, $b`), so a parameterised anonymous sub
  still renders as a `PointyBlock` — a documented divergence, unfixable without a distinguishing
  flag in the internal AST. `is rw` blocks remain the boundary.
- **Class and method declarations (Phase 2 slice 13).** `class NAME { body }` (`Stmt::ClassDecl`)
  → `Statement::Expression(Class(name => Name, body => Block(Blockoid(StatementList))))` — the class
  body is an ordinary `Block` whose statements convert recursively. `method NAME (params) { body }`
  (`Stmt::MethodDecl`) inside → `RakuAST::Method`, built by the same `routine_node` helper as `Sub`
  (parameters carry the implicit `type => Type::Setting(Any)`). Boundary (explicit `RuntimeError`):
  inheritance (`is`/`does`), `my`/`unit` scope, `repr`, `rw`, class traits; and for methods,
  private/submethod/multi/`our`/`my` forms, traits, delegation (`handles`), and return types.
  Roles are handled (slice 16). **Grammars are NOT mappable**: mutsu models `grammar G { }` as a
  `Stmt::ClassDecl` with `parents = ["Grammar"]` (no distinct grammar node), so it hits the class
  inheritance boundary rather than producing `RakuAST::Grammar`; deferred.
- **Role declarations (Phase 2 slice 16).** `role NAME { body }` (`Stmt::RoleDecl`) →
  `Statement::Expression(Role(name => Name, body => RoleBody(body => Blockoid(StatementList))))`.
  Unlike a class, the body is wrapped in a distinct `RoleBody` node (not a plain `Block`); its
  statements convert recursively (methods → `Method`, etc.). Boundary (explicit `RuntimeError`):
  parameterised roles (`role R[::T]`), `is export`, `is rw`, and traits.
- **Attributes (Phase 2 slice 14).** `has [Type] $.x` (`Stmt::HasDecl`) → a `VarDeclaration::Simple`
  with `scope => "has"` and a `twigil` leaf (`.` public accessor / `!` private), reusing the slice-10
  `var_declaration` helper (extended with a `twigil` field, and its initializer arg generalised to
  `Option<&Expr>`). A **typed** attribute (`has Int $.z`) carries an *implicit*
  `BareWord(<TypeName>)` default in mutsu's AST that is NOT an initializer and is filtered out
  (compared against the type name); an **explicit** default (`has $.x = 5`) becomes a
  `Trait::WillBuild` in raku (not an `initializer`), so it is deferred, along with `is rw`, type
  smileys (`:D`), `is required`, `where`, aliases (`has $x`), `my`/`our` attributes, delegation, and
  other traits.
- **Hyper method calls (Phase 2 slice 26).** `@a>>.abs` (`Expr::HyperMethodCall`) → `ApplyPostfix(
  operand, postfix => MetaPostfix::Hyper(Call::Method(...)))` — the inner `Call::Method` /
  `Call::QuotedMethod` is built by the same `method_call_postfix` helper as a plain method call, then
  wrapped in a `MetaPostfix::Hyper`. Other hyper forms (`<<.m`, `>>.m<<`, hyper infix `>>+<<`) are
  the boundary.
- **Reduction metaoperator (Phase 2 slice 25).** `[+] @a` (`Expr::Reduction`) → `Term::Reduce(
  triangle => False, infix => Infix("+"), args => ArgList(@a))`. The triangle form `[\+] @a`
  (mutsu stores its op as `"\+"`) sets `triangle => True` with the backslash stripped from the infix.
- **List-associative infixes (Phase 2 slice 24).** `andthen` / `orelse` / `notandthen` render as a
  single flat `ApplyListInfix(infix => Infix("andthen"), operands => (...))` in raku, not the
  binary `ApplyInfix` used by ordinary operators. mutsu parses them left-associatively
  (`a op b op c` → `(a op b) op c`), so the converter flattens a same-operator left chain into one
  operand list.
- **Quoted method names (Phase 2 slice 23).** `$x."foo"()` (`Expr::MethodCall` with `quoted ==
  true`) → `ApplyPostfix(operand, postfix => Call::QuotedMethod(name => QuotedString, [args =>
  ArgList]))` — unlike `Call::Method`, the name is a `QuotedString` (string literal) rather than a
  `Name.from-identifier`. A quoted name combined with a `.?`/`.+`/`.*` modifier is the boundary.
- **Positional subscripts (Phase 2 slice 22).** `@x[EXPR]` (`Expr::Index` with
  `is_positional == true`) → `ApplyPostfix(operand, postfix => Postcircumfix::ArrayIndex(index =>
  SemiList(Statement::Expression(EXPR))))`. Associative subscripts (`%h{...}` / `%h<...>`) are
  deferred: mutsu marks both `is_positional == false` with no `<>`-vs-`{}` distinction, so it cannot
  tell `<k>` (raku `LiteralHashIndex` + a word-quoted `QuotedString`) from `{"k"}` (raku
  `HashIndex` + `SemiList`).
- **Attribute build-time defaults (Phase 2 slice 27).** An explicit `has $.x = 5` default (deferred
  in slice 14) now emits BOTH a `traits => (Trait::WillBuild(5),)` field and an `initializer =>
  Initializer::Assign(5)`, matching raku. The implicit `BareWord(<TypeName>)` default of a typed
  attribute (`has Int $.z`) is still filtered out and produces neither. `var_declaration` gained a
  `will_build` parameter for the trait.
- **Ternary `?? !!` (Phase 2 slice 19).** `COND ?? THEN !! ELSE` (`Expr::Ternary`) →
  `Ternary(condition, then, else)`. raku constant-folds a literal-condition ternary (`1 ?? 2 !! 3`
  → `IntLiteral(2)`) while mutsu does not — the same const-fold divergence tracked in Open questions;
  a non-constant condition renders identically.
- **given / when / default (Phase 2 slice 18).** `given X { ... }` (`Stmt::Given`) →
  `Statement::Given(source, body => topic Block)` (the given body is a topic-taking `Block`, marked
  `implicit-topic`/`required-topic`). `when Y { ... }` (`Stmt::When`) → `Statement::When(condition,
  body => plain Block)`; `default { ... }` (`Stmt::Default`) → `Statement::Default(body => plain
  Block)`. All reuse the existing `topic_block_node`/`block_node` helpers.
- **Method-call modifiers (Phase 2 slice 15).** `$x.?foo` / `$x.+foo` / `$x.*foo` (`Expr::MethodCall`
  with a `modifier: Option<char>`) add a `dispatch => ".?"` / `".+"` / `".*"` leaf to the
  `Call::Method`, after `name` and any `args`. A quoted method name (`."$n"()`) remains the boundary.
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
  boundary (explicit `RuntimeError`). A `:D`/`:U` definiteness smiley (`my Int:D $x`) is handled
  (slice 20): the `type` field becomes `Type::Definedness(base-type => Type::Simple(Name), definite
  => True/False)` via the shared `build_type_node` helper. Parameterised types (slice 21):
  `Array[Int]` / `Hash[Str, Int]` → `Type::Parameterized(base-type => Type::Simple, args => ArgList(
  <type args>))`, with each argument built recursively by `build_type_node` (so nesting composes).
  Coercion types (slice 29): `Int()` → `Type::Coercion(base-type => Type::Simple(Int))`; a coercion
  with an explicit target (`Str(Int)`) and `:_` types still defer.
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
