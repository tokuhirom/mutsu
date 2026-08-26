# ADR-0033: Whatever-priming is a leaf property plus a derived scope — defer `WhateverCode` construction out of the parser

- **Status**: Phase 1 shipped (2026-08-19). Phase 2 shipped (2026-08-20, see "Phase 2
  outcome" below). **Phase 4 shipped (2026-08-23, see "Phase 4 outcome" below)** — the
  thunk-barrier priming correctness fix, plus its chained-comparison prerequisite. The
  chained-comparison `Expr::ChainedCompare` node the Phase 4 outcome deferred to
  `todo/tickets/chained-compare-ast-node.md` **shipped 2026-08-26** (see `news/2026-08/
  chained-compare-ast-node.md`): `TokenKind::ChainAnd` is retired now that a real AST node
  exists, and `.AST` renders a chain as rakudo's left-nested `ApplyInfix` instead of the
  expanded `&&`/`DoBlock` shape. Phase 3 (RakuAST write / `EVAL`) not started; it has no
  roast or correctness payoff of its own and was deliberately left until after Phase 4.
- **Scope**: Owns the `WhateverCode` item of
  [`todo/deep/rakuast-remaining.md`](../../todo/deep/rakuast-remaining.md) — both its
  read-direction half ("`* + 1` has no `.AST`") and its lowering half ("`EVAL` of a
  `WhateverCode` tree") — plus the pre-existing thunky-operator priming bug that shares
  the same root cause. Subordinate to
  [ADR-0011](0011-rakuast-model-layer-and-phasing.md): RakuAST stays a reflection/model
  layer, lowering through the existing internal AST and compiler, with no second
  execution engine.

## Context

Two problems that look unrelated turn out to have one root cause.

### Problem 1 — `* + 1` is invisible to RakuAST

`Q[* + 1].AST` is one of the highest-frequency constructs in real Raku code
(`.map(* + 1)`, `.grep(* > 3)`, `.sort(*.key)`, `@a[* - 1]`), and mutsu cannot model it
in either direction:

```
$ mutsu -e 'say Q[* + 1].AST'
RakuAST: `.AST` does not yet support this construct: Whatever-code closure

$ mutsu -e 'say Q[* + *].AST'
RakuAST: `.AST` does not yet support this construct: `is rw` / Whatever / typed pointy block
```

Those two errors are raised at `src/rakuast/convert.rs:1107` and `:1120`, in the
`Expr::Lambda { is_whatever_code, .. }` / `Expr::AnonSubParams { is_whatever_code, .. }`
arms. They fire because by the time the converter runs, `* + 1` is no longer an
expression containing a `*` — mutsu's **parser** has already replaced it with a
synthesized closure.

### Problem 2 — mutsu primes across thunky operators; raku does not

This is a live, user-visible correctness divergence, independent of RakuAST. Measured
against the system `raku`:

| expression | raku | mutsu |
|---|---|---|
| `(* > 3 && * < 8).arity` | `1` | `2` |
| `(* > 3 && * < 8)(5)` | `True` | `Too few positionals passed; expected 2 arguments but got 1` |
| `(1..10).grep(* > 3 && * < 8)` | `1 2 3 4 5 6 7` | `5 6` |
| `(*.defined && *.Str)(7)` | `7` | `Too few positionals passed` |
| `(* + 1 && 5).WHAT` | `(Int)` | `(WhateverCode)` |
| `(* + 1 and * + 2).arity` | `1` | curries the whole expression |
| `(* + 1 orelse * + 2).arity` | `1` | curries the whole expression |
| `(* // 5)(Nil)` | `No such method 'CALL-ME' for invocant of type 'Whatever'` | `5` |
| `(* + 1 ?? * + 2 !! * + 3).WHAT` | `(WhateverCode)` | dies coercing `Whatever` to `Numeric` |

Raku's rule is that `&&`, `||`, `//`, `and`, `or`, `andthen`, `orelse`, `notandthen` and
the ternary's three parts are **thunks**, and Whatever-priming happens *per thunk*. So
`* > 3 && * < 8` is two independent arity-1 `WhateverCode`s; `&&` then runs at that
expression's own evaluation time, sees a truthy `Code` object on the left, and returns
the right-hand `WhateverCode`. mutsu instead primes straight through `&&` and produces a
single arity-2 closure.

The ternary is the same bug seen from the other side. `Expr::Ternary` appears in **no**
arm of `contains_whatever` / `count_whatever` / `replace_whatever_*`, so it falls to
`_ => false` and mutsu primes *nothing* — neither the whole expression nor the individual
parts. The branches are also parsed by `ternary_no_assign` / `ternary_mode` /
`call_arg_ternary_expr` (`src/parser/expr/precedence/ternary.rs:265-284,365-401`) rather
than by `expression()`, so the per-`expression()` wrap does not run inside them either. A
bare `Expr::Whatever` survives into the tree and dies at runtime. Per-thunk priming fixes
both directions with one rule.

### Root cause

mutsu primes in the **parser**, eagerly, by building the closure on the spot.
`wrap_whatevercode()` (`src/parser/expr/whatever_wrap.rs:202`) is called from roughly
fifty sites across thirteen parser files —
`src/parser/expr/mod.rs` (six sites), `precedence/list_infix.rs`,
`precedence/list_infix_top.rs`, `precedence/chain_cmp.rs`, `precedence/comparison.rs`,
`precedence/assign.rs`, `expr/postfix/loop_.rs`, `primary/container/meta_ops.rs`,
`primary/ident/identifier_call.rs`, `primary/regex/call_args.rs`, `stmt/args.rs`,
`stmt/assign/compound_expr.rs` — each guarded by its own local combination of
`should_wrap_whatevercode()` / `contains_whatever()` (`src/parser/expr/whatever.rs`).

Two consequences follow directly:

1. **The pre-curry expression is destroyed before any consumer sees it.** The RakuAST
   converter, `.DEPARSE`, and error messages all receive an `Expr::Lambda { param: "_",
   is_whatever_code: true, body: [...] }` in which the `*` has already been rewritten to
   `$_` / `__wc_N`. Nothing can be recovered from it without guessing — and ADR-0011 is
   explicit that recovering a desugared construct "requires preserving the distinction in
   the parser/internal AST, not guessing during RakuAST conversion".
2. **No single place owns the priming-scope rule.** Today the scope is implicitly "one
   whole `expression()` call": the six entry points in `src/parser/expr/mod.rs`
   (`expression` `:117-136`, `expression_no_assign` `:179-195`,
   `expression_no_word_logical` `:240-256`, `expression_no_sequence` `:292-308`,
   `wrap_finished_expr` `:317-336`, `call_arg_expr` `:442-455`) each repeat the same
   twelve-line wrap block, and the comma is a boundary only because comma lists are
   assembled *above* `expression` (`src/parser/stmt/assign/comma.rs:24,39,81,93`). Every
   other boundary is a hand-written opt-out arm. The barrier set is whatever
   `should_wrap_whatevercode` and `contains_whatever` happen to encode at
   `src/parser/expr/whatever.rs:11-62` and `:160-291` — ranges, series, `xx`, `o`,
   smartmatch, colonpairs, flip-flops, and six non-currying pseudo-methods are all
   handled there, one arm at a time. The generic `Expr::Binary { left, right, .. }` arm
   (`whatever.rs:218`) curries through *every* remaining operator, which is exactly why
   `&&` / `||` / `//` / `and` / `or` / `andthen` / `orelse` were never excluded: nobody
   had to write them down.

### How Rakudo models it (measured, not assumed)

`raku -e 'say Q[...].AST'` shows two facts that shape this decision:

- **The `*` is recorded at the leaf.** `RakuAST::Term::Whatever` is a `*` that stays a
  `Whatever` *value*; `RakuAST::WhateverCode::Argument` is a `*` that is a priming
  *argument*. `1, *, 2` and `1..*` produce `Term::Whatever`; `* + 1`, `*.abs` and
  `1..*-1` produce `WhateverCode::Argument`. The distinction is decided by the operand's
  immediate context, i.e. it is leaf-local.
- **There is no wrapper node for the closure.** `* + 1` is simply
  `ApplyInfix(WhateverCode::Argument, Infix("+"), IntLiteral(1))`. `(* + 1) * 2` is an
  `ApplyInfix` whose left operand is a `Circumfix::Parentheses`. `* > 3 && * < 8` is a
  plain `ApplyInfix` with `Infix("&&")` and a `WhateverCode::Argument` inside each
  operand. Rakudo therefore *derives* the closure boundary at lowering, from the
  enclosing structure, and the RakuAST tree carries no priming scope at all.

## Decision

Adopt Rakudo's leaf/scope split, with one adaptation for mutsu's parser.

### 1. Split the `*` leaf

`Expr::Whatever` keeps its current meaning — "a `Whatever` value" — and keeps compiling
to `LoadConst(Value::WHATEVER)` (`src/compiler/expr.rs:51`). A new sibling variant

```rust
Expr::WhateverArg
```

marks a `*` that participates in priming. The decision of which leaf to emit is exactly
the exception list in `raku-doc/doc/Type/Whatever.rakudoc:44-55` plus mutsu's existing
arms, and it is decidable from the operand's immediate parent: a **bare** `*` directly in
a comma operand, a `..`/`^..`/`..^`/`^..^` endpoint, a `...`/`...^` operand, an
assignment or `:=` right-hand side, an `xx` operand, `x`'s right operand, a `ff`/`fff`
operand, or a call/method *argument* position is `Expr::Whatever`; anything else is
`Expr::WhateverArg`. The predicates in `src/parser/expr/whatever.rs` are not deleted —
they are re-aimed from "should I wrap this subtree" to "is this `*` an argument".

`Expr::HyperWhatever` (`**`) is out of scope: it never curries today (`is_whatever` at
`whatever.rs:135-137` matches only `Expr::Whatever`, and `HyperWhatever` appears in no
arm of `contains_whatever`/`count_whatever`) and it has no `convert.rs` arm either, so it
keeps hitting the catch-all `unsupported` error. Likewise `wrap_composition_operands`
(`whatever_wrap.rs:7-121`) deliberately builds `is_whatever_code: **false**` lambdas for
`o` / `∘` operands — those are plain closures, not `WhateverCode`s, and that stays true.

### 2. Mark the priming scope with a marker node, not a closure

```rust
Expr::WhateverCurry(Box<Expr>)
```

wraps a maximal priming scope. It carries the **un-curried body**, with
`Expr::WhateverArg` leaves still in place. It is a marker only — not a closure, not a
`Code` value, and it never reaches the VM as itself.

*Why mutsu needs a marker where Rakudo does not.* Rakudo can derive the scope purely
structurally because its grammar preserves parentheses as `Circumfix::Parentheses`.
mutsu's parser drops parentheses in several positions — ADR-0011's Open questions
already record that "a **parenthesised** list `(1, 2)` is `Circumfix::Parentheses(...)`
in raku, but mutsu drops the parens at parse time" — so `(* + 1) * 2` and `* + 1 * 2`
are not reliably distinguishable from the internal AST alone. Deriving the scope would
therefore be a guess, which ADR-0011 forbids. The marker also makes the transform
idempotent and gives the RakuAST lowerer a place to state the scope it derived.

If mutsu later preserves `Expr::Grouped` universally, `WhateverCurry` becomes redundant
and can be retired. That is a strictly-later simplification, not a prerequisite.

### 3. Move closure construction to the compiler

`wrap_whatevercode`, `replace_whatever_single`, `replace_whatever_numbered`,
`count_whatever`, `make_wc_param` and `expr_contains_topic` move out of
`src/parser/expr/` into a new parser-independent module **`src/whatever_curry.rs`**.
`src/placeholder_order.rs` is the precedent: a post-parse AST transform that lives at
crate root and is shared by more than one consumer.

`src/compiler/expr.rs` gains one arm:

```rust
Expr::WhateverCurry(body) => {
    let closure = whatever_curry::build_closure(body);   // the old wrap_whatevercode
    self.compile_expr(&closure);                          // existing Lambda / AnonSubParams path
}
```

`build_closure` returns the same `Expr::Lambda { param: "_", is_whatever_code: true, .. }`
or `Expr::AnonSubParams { params: ["__wc_0", ..], is_whatever_code: true, .. }` that the
parser builds today, so **the emitted bytecode is identical**. No new `OpCode`, no new
runtime path, no `runtime/methods.rs` fallback, no second execution engine.

`is_whatever_code` survives as an internal property of the *generated* closure; it simply
stops being something the parser writes into the durable AST. Everything downstream of
the compiler is untouched, which matters because the flag carries real semantics:

- `src/compiler/expr_closure.rs:183` — a WhateverCode skips the placeholder-vs-explicit-
  signature conflict check (it owns only its `*`-derived params, so a `$^name` in the body
  belongs to the enclosing block).
- `:262-264`, `:377-379` — `collect_whatever_expr_decls` hoists `my`-declared names out of
  the closure body.
- `:349` — `wc_raw`: the `_` parameter gets the `raw` trait when the body mutates the
  topic, so `*++`, `*.=foo` and `* =:= $x` write back to the caller's container.
- `:406-408` — a WhateverCode is not marked `is_pointy_block`.
- `:438` — the flag rides in `OpCode::MakeLambda` / `MakeAnonSubParams`, and
  `src/vm/vm_register_sub_ops.rs:99-104` / `vm_register_ops.rs:341-346` turn it into the
  `__mutsu_callable_type = "WhateverCode"` env marker (stripped from captured envs at
  `vm_register_ops.rs:600-612`).
- That marker is what makes `.WHAT.^name` report `WhateverCode`
  (`src/runtime/methods_introspect.rs:123`), exempts the closure from the routine-entry
  `$_ = Any` reset (`src/vm/vm_closure_dispatch.rs:625-634`), and drives the
  `.map`/`.grep` topic binding (`src/runtime/resolution_map_grep.rs:247-254,430,489,507`).

Phase 1 changes none of this: it only moves *when* `build_closure` runs.

### 4. One scope authority

A single function

```rust
whatever_curry::plant(expr: Expr) -> Expr
```

wraps each maximal priming scope in `Expr::WhateverCurry`. It has exactly two callers:
the parser (once per statement/argument expression) and `src/rakuast/lower.rs` (to
re-derive the scope of a RakuAST tree, which carries no scope marker). Its rule — the
decision this ADR fixes — is:

- **Value-leaf positions** — no priming: the `Expr::Whatever` cases from §1.
- **Thunk barriers** — each operand is its own priming scope: `&&`, `||`, `//`, `and`,
  `or`, `andthen`, `orelse`, `notandthen`, and each of the ternary's condition / then /
  else. (`xor` is deliberately **not** on this list; see Risks.) For the ternary this is a
  *gain*, not merely a restriction: mutsu primes nothing there today, so per-thunk
  planting is what makes `* + 1 ?? * + 2 !! * + 3` produce a `WhateverCode` at all.
- **Structural barriers**: `Expr::Grouped`, a call or method argument list, a
  statement / `SemiList` boundary, and an already-planted `WhateverCurry`.
- **Non-currying pseudo-methods**: `.WHAT` / `.WHO` / `.HOW` / `.WHERE` / `.DEFINITE` /
  `.VAR` applied to a `*` or to a planted curry evaluate eagerly (already encoded at
  `src/parser/expr/whatever.rs:231-238`; preserved verbatim).
- **Composition `o` / `∘` and smartmatch** keep their existing opt-outs
  (`whatever.rs:23-37`, `:205-208`) — they are barriers of the same family.
- Everything else is transparent: walk up.

### 5. RakuAST read direction

In `src/rakuast/convert.rs`:

- `Expr::WhateverCurry(body)` converts to `convert_expr(body)` with **no wrapper node**,
  matching Rakudo.
- `Expr::WhateverArg` converts to a new `RakuAstClass::WhateverCodeArgument`, printed
  `RakuAST::WhateverCode::Argument` and rendered as a bare `.new` via the existing
  `empty_parens_omitted` path that `RakuAstClass::TermWhatever` already uses
  (`src/rakuast/mod.rs:239`).
- `Expr::Whatever` keeps mapping to `RakuAstClass::TermWhatever`
  (`convert.rs:828`).
- The `is_whatever_code` guards at `convert.rs:1107` and `:1120` are deleted: the parser
  no longer produces those closures, so the remaining arms handle only genuine pointy
  blocks and anonymous subs.
- Hierarchy metadata: `WhateverCodeArgument` is a `Term` and an `Expression`, listed in
  the `semantic_ancestors` table (ADR-0011 Phase 3 slice 4) next to `TermWhatever`.

### 6. RakuAST write direction

In `src/rakuast/lower.rs`:

- `RakuAstClass::WhateverCodeArgument` lowers to `Expr::WhateverArg` (mirroring
  `TermWhatever => Expr::Whatever` at `lower.rs:655`).
- After lowering a statement's expression, run `whatever_curry::plant` on the result. This
  closes an asymmetry that exists today and would otherwise persist: the converter
  *refuses* a WhateverCode while the lowerer emits a bare `Expr::Whatever` and never
  re-runs the currying transform (which lives entirely in the parser), so even a
  hypothetical round-trip would come back as an uncurried `Binary{Plus, Whatever, 1}`.
  With `plant` shared, both directions agree. Concretely, it makes
  a **hand-constructed** tree
  `RakuAST::ApplyInfix.new(left => RakuAST::WhateverCode::Argument.new, infix =>
  RakuAST::Infix.new("+"), right => RakuAST::IntLiteral.new(1))` EVALs to the same
  closure a parsed `* + 1` produces. This is the reason `plant` must be shared and
  parser-independent rather than living in `src/parser/`.

## Phasing

Each phase is independently shippable and CI-gated. Phases 2 and 3 do not depend on
phase 4, and phase 4 does not depend on RakuAST at all.

- **Phase 1 — deferral, behaviour-preserving.** Add `Expr::WhateverArg` and
  `Expr::WhateverCurry`; create `src/whatever_curry.rs`; add the compiler arm. **Keep the
  existing scope sites**: each current `wrap_whatevercode(&e)` call becomes
  `Expr::WhateverCurry(Box::new(e))` and each `should_wrap_whatevercode` gate stays as
  written, so the scope decision — and therefore every runtime result — is unchanged by
  construction. The read-only consumers of the flag
  (`src/placeholder_order.rs:364,660`, `is_wrapped_whatevercode` at
  `src/parser/expr/whatever.rs:147`, `src/parser/expr/postfix/helpers.rs:124-150`) match
  `Expr::WhateverCurry` instead. Rust's exhaustive `match` over `Expr` turns every missed
  site into a compile error, which is the mechanism that makes a fifty-site rewrite safe.
- **Phase 2 — RakuAST read.** The `convert.rs` mapping of §5, pinned by a dual-oracle
  `t/rakuast-whatever-code.t` (must pass under **both** mutsu and raku, per the
  ADR-0011 convention): `* + 1`, `* + *`, `*.abs`, `1..*` vs `1..*-1`, `1, *, 2`,
  `(* + 1) * 2`, `@a[* - 1]`.
- **Phase 3 — RakuAST write / EVAL.** The `lower.rs` mapping of §6, pinned by
  `t/rakuast-eval-whatevercode.t`: `EVAL(Q[(1,2,3).map(* + 1).sum].AST)` → `12`,
  `EVAL(Q[(1..10).grep(* > 5).elems].AST)` → `5`, and a hand-constructed
  `WhateverCode::Argument` tree that EVALs to a working closure. This closes the
  "WhateverCode" boundary listed in ADR-0011 Phase 5.
- **Phase 4 — one scope authority + thunk barriers.** Replace the ~50 parser sites with a
  single `plant` call per statement/argument expression, and switch on the thunk-barrier
  set of §4. This is the correctness phase; pin it with `t/whatever-thunky-operators.t`
  covering every row of the Problem-2 table. It also requires the chained-comparison
  fix below.

### Phase-4 prerequisite: chained comparison must stop synthesizing a bare `&&`

`src/parser/expr/precedence/chain_cmp.rs` expands `a < m < b` into
`(a < m) && (m < b)` with the middle operand duplicated —
`count_whatever`'s `TokenKind::AndAnd` special case
(`src/parser/expr/whatever.rs:309-336`) exists solely to undo that duplication when
computing arity, which is how `1 < * < 10` correctly yields arity 1 today. Making `&&` a
thunk barrier would break that, because the synthesized `&&` is not a user-written one.

Rakudo does not have this problem: `Q[1 < * < 10].AST` is a left-nested
`ApplyInfix(ApplyInfix(1, "<", WhateverCode::Argument), "<", IntLiteral(10))` — the
chaining semantics come from the operator's chaining precedence at code-gen, not from an
AST-level `&&`. Phase 4 must therefore make the expansion distinguishable: either plant
the curry before the chain expansion runs, or give the expansion its own node
(`Expr::ChainedCompare { operands, ops }`) that `plant` treats as transparent. The
second is preferable — it also lets phase 2 render `1 < * < 10` faithfully instead of as
the expanded `&&`, which would otherwise be a fresh documented divergence in the same
family as `unless` → `if !`.

## Alternatives considered and rejected

- **(A) Reconstruct the pre-curry expression inside the RakuAST converter** by walking the
  generated closure and replacing `$_` / `__wc_N` back with `WhateverCode::Argument`.
  *Rejected*: this is precisely the "guess during RakuAST conversion" ADR-0011 rules out.
  It cannot distinguish a hand-written `-> $x { $x + 1 }` from a curried `* + 1` beyond
  the `is_whatever_code` flag, it cannot recover the parentheses of `(* + 1) * 2`, and it
  leaves Problem 2 completely unaddressed.
- **(B) Carry the pre-curry body as an extra field on the generated closure**
  (`Expr::Lambda { whatever_source: Option<Box<Expr>>, .. }`). *Rejected*: it stores the
  same subtree twice, so the two copies drift — every later AST rewrite (placeholder
  ordering, sink-warning analysis, constant handling) must remember to rewrite both, and
  a missed one is a silent wrong `.AST` rather than a compile error. It also leaves the
  scope decision smeared across fifty sites, so phase 4 remains impossible without
  redoing the work.
- **(C) No marker node — derive the scope purely structurally, exactly as Rakudo does.**
  *Rejected for today* because mutsu's parser drops parentheses in several positions, so
  `(* + 1) * 2` and `* + 1 * 2` are not always distinguishable. Revisit if `Expr::Grouped`
  ever becomes universal; then `WhateverCurry` is deleted and `plant` derives.
- **(D) Fix only the thunk barriers, leave priming in the parser.** *Rejected*: it buys
  the correctness gain but leaves the RakuAST item permanently unreachable, and the next
  agent re-does this whole investigation. The thunk fix is also *cheaper* on top of the
  deferral, because `plant` gives it one place to live instead of fifty guards to keep in
  sync.
- **(E) A dedicated `OpCode::MakeWhateverCode`.** *Rejected*: it would be a second
  construction path for something the existing closure opcodes already express, against
  both CLAUDE.md's "do not add new slow-path fallbacks" and ADR-0011's "no second
  execution engine". `OpCode` is also under a 48-byte size guard, so new variants are not
  free.

## Risks

- **Phase-1 blast radius.** Roughly fifty call sites across thirteen parser files change
  shape. Mitigation: every one is a mechanical substitution, `Expr` is matched
  exhaustively so omissions are compile errors, and the whitelisted
  `roast/S02-types/whatever.t` and `roast/S03-operators/composition.t` exercise this
  surface densely. Per CLAUDE.md's "gain and risk" definitions, a temporary red CI on the
  branch is the safety net working, not a cost.
- **Phase 4 changes results.** Any code that relied on mutsu currying across `&&` will
  behave differently. Since mutsu's current result already differs from raku's, such
  reliance is a bug by definition; roast is the arbiter. Expect to audit
  `roast/S02-types/whatever.t` and the `t/whatever*.t` files as part of the phase.
- **`xor` is unresolved.** raku's `* + 1 xor * + 2` yields `Nil` with a
  "Useless use of `+` in sink context" warning — neither a `WhateverCode` nor a plain
  `Bool`. `xor` is not short-circuit, so it is not obviously a thunk barrier. This ADR
  deliberately excludes it; phase 4 must re-measure against rakudo rather than guess, and
  record the outcome here.
- **`.arity` composition.** `(* + 1 == * + 2).arity` renders `WhateverCode.new` in
  *both* implementations (the `.arity` call is itself inside the priming scope), while
  `(* + 1 orelse * + 2).arity` is `1` in raku because the barrier ends the scope before
  `.arity`. `plant` must get the interaction between barriers and trailing postfixes
  right; both forms belong in `t/whatever-thunky-operators.t`.
- **File-size cap.** `src/rakuast/convert.rs` (1795 lines), `mod.rs` (1048) and
  `lower.rs` (864) already exceed the repo's 500-line convention, as does
  `src/parser/expr/mod.rs` (515), which hosts six of the wrap sites. Phase 1's move gives
  a natural opportunity to shrink `src/parser/expr/mod.rs`; phases 2 and 3 should land
  *with* a per-cluster split of the RakuAST files, not stacked on top of them.
  `src/whatever_curry.rs` itself will be roughly the size of today's `whatever_wrap.rs` +
  `whatever_replace.rs` (~730 lines), so it should be created as a directory module
  (`src/whatever_curry/{mod,build,replace,plant}.rs`) from the start.

## Outcome

**Phase 1 shipped (2026-08-19).** `Expr::WhateverArg` and `Expr::WhateverCurry` were added
to the AST; `src/whatever_curry/` (`mod.rs`, `build.rs`, `replace.rs`) now owns closure
construction (`build_closure`, formerly `wrap_whatevercode`), placeholder replacement, and
arity counting, invoked from exactly one place — the new `Expr::WhateverCurry` arm in
`Compiler::compile_expr`. Every parser-side `wrap_whatevercode(&e)` call site now
constructs `Expr::WhateverCurry(Box::new(e))` instead; `should_wrap_whatevercode` /
`contains_whatever` (the scope decision) are byte-for-byte unchanged, so — as designed —
this is a pure deferral with no behaviour change. `Expr::WhateverArg` is defined but not
yet produced anywhere (that is Phase 2's leaf-splitting work); it compiles identically to
`Expr::Whatever` so the arm is reachable-but-inert until then.

`whatever_curry/{mod,build,replace}.rs` is a directory module as the Risks section
anticipated; `plant.rs` is deferred to Phase 4, which is what actually needs a
single-authority scope function (Phase 1 keeps the scope decision distributed across the
existing ~50 call sites on purpose, per the "behaviour-preserving by construction"
mandate).

The mechanical site-by-site rewrite surfaced substantially more than the ~50 originally
counted `wrap_whatevercode` call sites: every other parser/compiler/runtime pass that
pattern-matched the *eagerly built* `Lambda { is_whatever_code: true, .. }` /
`AnonSubParams { is_whatever_code: true, .. }` shape to detect "this subtree is already a
WhateverCode" needed an `Expr::WhateverCurry` arm added too, since that shape no longer
exists at parse time. Two of these were genuine correctness regressions caught before
merge (both fixed in this same PR, not deferred):

- `crate::ast::collect_ph_expr_shallow` (the placeholder-order collector that decides a
  block's own implicit signature) didn't hoist a `$^name` placeholder out of a nested
  WhateverCurry into the enclosing block's signature, undercounting its arity by one.
  Reproduced via the YAMLish battery's `flatten-tags` helper
  (`{ |$^value.kv.map($^namespace ~ * => *) }`), which silently mis-bound values before the
  fix (see `t/placeholder-in-nested-whatevercode.t`, already pinning a simpler shape of the
  same bug).
- The expression-context `:=` bind fast path (`parser/expr/precedence/logic.rs`) and its
  compiler-side `X::Bind::Slice` throw (`compiler/expr_closure.rs`) both matched only the
  built `Lambda`/`AnonSubParams` shape to detect a Whatever-index bind (`@a[*-1] := 42`),
  so it silently took the *valid*-bind fast path instead of throwing. Pinned by the
  pre-existing `t/bind-to-whatever-index.t` / `t/indexed-bind-in-expression.t`.

Roughly a dozen more sites (`outer_redecl.rs`, `sink_warn.rs`, `whenever_scope.rs`,
`stmt/modifier.rs`, `stmt/class/attr_checks.rs`, `stmt/sub/param_validate.rs`,
`primary/container/paren.rs`, `runtime/{undeclared_routines,system_eval_names,phasers,
registration,registration_class_attr}.rs`, `runtime/types/type_matching.rs`'s `subset
... where <WhateverCode>` predicate-callable check, and `compiler/{expr_call,
helpers_call_args}.rs`'s `cas($var, * + delta)` atomic-add fast path and closure-escape
detection) got the same treatment as a precaution — each recurses into the un-expanded
`WhateverCurry` body the same way it already recursed into a real closure body, so a
diagnostic, validation, or fast-path check that used to see the built closure still sees
the (structurally equivalent, pre-substitution) un-curried one. None of these changed
observable behaviour relative to `main`; they close latent gaps the deferral would
otherwise have opened. A few sites were deliberately left alone: `stmt/assign/
compound_expr.rs` and `precedence/{assign,comparison}.rs`'s hand-rolled `* op= value` /
`* ~~ Type` / `Type ~~ *` autoprime constructions never called `wrap_whatevercode` (they
build a closure directly), so leaving them eager is consistent with "keep the existing
scope sites" and does not block Phase 2 (that already-eager path is no worse than
`main`'s); `runtime/registration_class_augment.rs`'s `does *~~Role`-style helper is the
same.

Validated locally: `cargo test --workspace` (all binaries, including `gc_stress` and
`lazy_match_no_eager_materialization`) green; the full `t/` TAP suite (3255 files) green
except the pre-existing, already-ticketed `t/autoviv-index-guard.t` local hang
(`todo/tickets/autoviv-index-guard-hangs-locally.md`, confirmed unrelated); `t/*whatever*.t`
(35 files), `roast/S02-types/{whatever,hyperwhatever}.t`, `roast/S03-operators/
composition.t`, and `roast/S12-subset/{multi-dispatch,subtypes,type-subset}.t` all green.

**Phases 2-4 are not implemented by this PR** — deliberately scoped out per the ADR's own
phasing ("Each phase is independently shippable and CI-gated"): Phase 1's mandate is a
zero-behaviour-change deferral, verified above; Phase 4 (the thunk-barrier correctness fix
that actually changes `&&`/`||`/`//`/ternary priming results) is a separate, higher-risk
change that deserves its own PR and its own `t/whatever-thunky-operators.t`. Phases 2/3
(RakuAST read/write for `* + 1`) remain open items in
[`todo/deep/rakuast-remaining.md`](../../todo/deep/rakuast-remaining.md).

## Phase 2 outcome (2026-08-20)

Implemented exactly per "Phase 2 detailed design" below. `src/whatever_curry/mark.rs` adds
`mark_program(&mut stmts)`, a single top-down walk invoked once from
`parser::parse_program` right after a program parses. It rewrites every `Expr::Whatever`
leaf to `Expr::WhateverArg` unless the leaf's immediate syntactic parent is one of the
value positions in section 2.1's table (comma operand, range/series endpoint, `xx`
operand, assignment/bind RHS, call/method argument, whole-slice subscript, non-currying
pseudo-method target, bareword-pair value, or a bare `*` standing alone as a whole
statement/grouping); everything else becomes `Argument`. The predicates are genuinely
scope-independent, not derived from `contains_whatever`/`should_wrap_whatevercode` (the
`1 x *` and `* Z 1`/`* X 1` counter-examples from section 2.1 are both classified
`Argument` correctly even though mutsu plants no `WhateverCurry` scope for them).

`crate::parser::is_whatever` widened to `matches!(expr, Expr::Whatever | Expr::WhateverArg)`
per section 2.2's invariant; the four remaining variant-literal `matches!(&**left/right,
Expr::Whatever)` tests in the `x`/`xx` arms (`whatever.rs`) now go through it. Verified by
construction (every scope/arity predicate is built on this one helper) and by the full
`t/*whatever*.t` (35 files) + `roast/S02-types/{whatever,hyperwhatever}.t` +
`roast/S03-operators/composition.t` + `roast/S12-subset/{multi-dispatch,subtypes,
type-subset}.t` sweep passing unchanged.

`src/rakuast/mod.rs` gained `RakuAstClass::WhateverCodeArgument`
(`"RakuAST::WhateverCode::Argument"`, bare `.new`) and `RakuAstClass::TermHyperWhatever`
(`"RakuAST::Term::HyperWhatever"`, bare `.new`); `WhateverCodeArgument` was added explicitly
to the `semantic_ancestors`/`semantic_type_object_ancestors` TERM set (the section 2.4 trap:
its printed name does not start with `RakuAST::Term::`, so the name-prefix rule would
otherwise silently make `~~ RakuAST::Term` false). `src/rakuast/convert.rs` gained arms for
`Expr::WhateverArg` (-> `WhateverCodeArgument`), `Expr::HyperWhatever` (-> `TermHyperWhatever`,
a read-direction-only bonus per section 2.4 — priming for `**` stays out of scope), and
`Expr::WhateverCurry(body)` (-> `convert_expr(body)`, no wrapper node, matching Rakudo's
scope-free tree).

Section 2.5's three remaining eager-closure sites were converted to plant
`Expr::WhateverCurry` instead of building a `Lambda` by hand: `* ~~ Type` and `Type ~~ *`
(`src/parser/expr/precedence/comparison.rs`, covering `~~` and `!~~` both). This required
re-checking the SmartMatch/BangTilde asymmetry the ADR's section 2.5 called out:
`count_whatever`/`replace_whatever_numbered`/`replace_whatever_single`
(`src/whatever_curry/{build,replace}.rs`) only counted/substituted the *left* operand
(the historical "Whatever on the RHS is runtime-autoprimed, not curried" rule for a
*compound* RHS); they now also count/substitute a *bare* right-hand placeholder, which is
exactly what the newly-planted `X ~~ *` marker needs. This incidentally fixed a latent
mutsu bug: `$_ ~~ *`'s closure previously replaced the *outer* `$_` too (so the closure
ignored the caller's dynamic topic), where raku's `-> $a { $_ ~~ $a }` reads the outer
topic on the left and only primes the right — verified against raku
(`$_ = 10; ($_ ~~ *)(3)` is `False`, i.e. `10 ~~ 3`, in both). The compound-assignment
family (`* += 1`) stays a boundary, per section 2.5's own scoping (needs a
`MetaInfix::Assign` class mutsu lacks — an operator-cluster-wide gap, not Whatever-specific).

Two adjacent RakuAST rendering bugs surfaced (and were fixed) because this section's
`~~`/`!~~`/`=>` constructs no longer die at the `.AST` boundary before reaching them:
`token_kind_to_op_name` (`src/compiler/helpers_ops.rs`) rendered `!~~` as the internal
two-tilde string `"!~"` and `=>` via its `{:?}` Debug fallback (`"FatArrow"`) — neither
is reachable from the compiled-bytecode dispatch path (`!~~`/`~~` compile via the
dedicated `OpCode::SmartMatchExpr`, never through the generic `InfixFunc` fallback that
calls this function), so fixing the strings to `"!~~"` / `"=>"` (and the reverse
`op_name_to_token_kind` lowering table) is display-only and safe.

Section 2.6's two divergences (chained comparison, `* .= lc`) are left exactly as
documented — not attempted.

New dual-oracle test: `t/rakuast-whatever-code.t` (68 assertions), passing verbatim under
both `target/debug/mutsu` and the system `raku`. Covers every row of section 2.7's test
plan except `[*]` (excluded with an inline comment: mutsu currently parses a standalone
`[*]` as the `[*]`-reduction metaoperator over an empty list — a pre-existing,
Whatever-unrelated parse ambiguity with no `*` node in the tree to classify either way —
while raku renders `Term::Whatever`; out of scope for this ADR).

## Phase 4 outcome (2026-08-23)

Shipped. Every row of the Problem-2 table at the top of this ADR now matches the system
`raku`, re-measured on the day rather than trusted from the table, and pinned by a new
**dual-oracle** `t/whatever-thunky-operators.t` (34 assertions, passing verbatim under
both `target/debug/mutsu` and `raku`).

### What actually changed — much less than section 4 anticipated

The design in section 4 assumed Phase 4 had to "replace the ~50 parser sites with a single
`plant` call". Measuring first showed that is not what the correctness fix requires, and
that doing it would have been a large *behaviour-neutral* refactor bolted onto a
behaviour-changing one. The rule decomposes into two halves, and the parser's existing
sites fall into line on their own once the first half is in place:

1. **A thunk barrier is opaque to the enclosing priming scope.** `contains_whatever`
   (`src/parser/expr/whatever.rs`), `count_whatever` (`whatever_curry/build.rs`) and both
   `replace_whatever_*` walkers (`whatever_curry/replace.rs`) stop dead at a barrier. This
   is Rakudo's own model: a thunky op is never "whatever-ish", so it neither creates nor
   enlarges a scope above it. Because every one of the ~50 parser planting sites is gated
   on `should_wrap_whatevercode` → `contains_whatever`, **no site can any longer propose a
   scope that spans a barrier** — the fifty-site rewrite turned out to be unnecessary for
   the correctness goal, not merely deferrable.
2. **Each barrier operand is a scope of its own.** The new `src/whatever_curry/plant.rs`
   owns this — `is_thunk_barrier()` (the barrier set) and `plant_here()` (materialise an
   `Expr::WhateverCurry` around each operand that primes, gated by the same
   `should_wrap_whatevercode`). It is invoked from the head of `mark_expr`
   (`whatever_curry/mark/expr.rs`), which section 2.3 already designated "deliberately the
   seed of the `plant.rs` this ADR's section 4 calls for — same traversal, same
   parent-context switch, one phase later it also decides where scopes begin". That
   prediction held exactly: no second walk was needed.

Half 1 is what makes the *residue* case fall out for free. `((* > 3 && * < 8) + *)` is a
single arity-1 `WhateverCode` in rakudo (measured); with the barrier opaque, the enclosing
`+` sees exactly one placeholder and the existing machinery curries it correctly, with no
special case. Half 2 is a strict gain for the ternary, which mutsu previously primed *not
at all*: all three parts are now scopes, so `(* + 1 ?? * + 2 !! * + 3)` is a
`WhateverCode` instead of dying while coercing `Whatever` to `Numeric`.

Because a barrier's operands are materialised *before* the enclosing closure body is
built, `replace_whatever_*` must clone a barrier subtree through untouched rather than
substituting `$_` / `__wc_N` into it — the inner markers are separate closures that the
compiler expands on its own when it compiles the outer lambda's body.

### The prerequisite: `TokenKind::ChainAnd`, not `Expr::ChainedCompare`

The "Phase-4 prerequisite" section below is confirmed real, and the measurement that
proves it also corrects section 2.6. That section states `(1 < * < 10).arity` "is 1 in
both implementations"; it is not — rakudo prints `WhateverCode.new`, because `.arity` is
itself inside the priming scope. The load-bearing measurement is instead the pair

```text
(1 < * < 10)(0)        False   # one arity-1 curry over the whole chain
(1 < * && * < 10)(0)   True    # a real `&&` yields only its right-hand thunk
```

which shows a synthesized chain conjunction and a user-written `&&` must land on opposite
sides of the barrier rule. The prerequisite section offers two resolutions; this PR takes
a third, lighter one that satisfies the same requirement ("make the expansion
distinguishable"): a dedicated `TokenKind::ChainAnd`, emitted by `chain_cmp.rs` and
`comparison.rs` where they synthesize the conjunction, absent from `is_thunk_barrier`, and
compiled exactly like `AndAnd`.

`Expr::ChainedCompare { operands, ops }` was costed and rejected *for this PR*. A new
`Expr` variant needs an arm in every walker that must see through it — Phase 1's own
Outcome records 41 files needing a `WhateverCurry` arm — and the failure mode is silent:
a walker with a `_ => {}` catch-all (placeholder collection, sink warnings, closure
free-variable analysis) would simply stop seeing the chain's operands. `ChainAnd` keeps
the `Expr::Binary` shape every existing walker already handles, so its audit is the
bounded set of 20 `TokenKind::AndAnd` sites, all inspected. The node form remains worth
doing for its *other* benefit — rendering `1 < * < 10` faithfully in RakuAST instead of as
the expanded `&&` — and was tracked as `todo/tickets/chained-compare-ast-node.md`, shipped
2026-08-26 (see the Status line above and
[`news/2026-08/chained-compare-ast-node.md`](../../news/2026-08/chained-compare-ast-node.md)).

A bonus correctness fix came with it: the `count_whatever` / `replace_whatever_numbered`
de-duplication special case, which used to fire on *any* `AndAnd` whose two operands
shared a structurally-equal middle, now fires only on `ChainAnd`. That heuristic was
mis-firing on a user-written `1 < * && * < 10`, which mutsu curried into one arity-1
closure (`(0)` → `False`); it now correctly yields the right-hand thunk (`(0)` → `True`,
matching rakudo).

### `xor` and `^^` re-measured, as the Risks section demanded

Both are non-short-circuit and rakudo primes neither: `(* + 1 xor * + 2).WHAT` and
`(* + 1 ^^ * + 2).WHAT` are each `Nil` plus a `Useless use of "+" in expression "* + 2" in
sink context` warning. No barrier treatment reproduces that (splitting would yield a
`Bool`, not `Nil`), so both stay off the barrier list and mutsu keeps returning
`(WhateverCode)` — a documented divergence rather than a guess. This resolves the "`xor`
is unresolved" risk: excluded, deliberately, with `^^` alongside it.

### Divergences left standing (all pre-existing, none touched by this phase)

`(1 xx *).WHAT` is `Array` vs rakudo's `Seq`; `((* - 1) o (* * 2)).WHAT` is `Sub` vs
`Block+{...}`; `(*(1))` curries in mutsu where rakudo dies with "No such method 'CALL-ME'
for invocant of type 'Whatever'". None involve a thunk barrier.

### Validation

`prove t/` in full (3361 files, 31562 assertions) green; `cargo test --workspace` (868
unit tests plus every integration binary) green; `cargo clippy -- -D warnings` clean; the
whitelisted `roast/S02-*`, `roast/S03-*` and `roast/S04-*` sweep (345 files) green, with
`roast/S02-types/{whatever,hyperwhatever}.t`, `roast/S03-operators/composition.t`,
`short-circuit.t` and `ternary.t` checked individually first.

## Phase 2 detailed design (added 2026-08-20)

Phase 1 shipped the *scope* half of this ADR's title (the `Expr::WhateverCurry` marker).
Phase 2 is the *leaf* half: making `Expr::WhateverArg` — added but inert by Phase 1 — actually
carry the "this `*` is a priming argument" property, and rendering it as
`RakuAST::WhateverCode::Argument`. This section fixes the four things §1/§5 left open: the
exact classification rule, where the classifier runs, what the rest of the interpreter is
allowed to see, and which divergences Phase 2 deliberately does not close.

Everything below was re-measured against the system `raku` and the current `main` build on
2026-08-20; the Problem-1 and Problem-2 tables at the top of this ADR still reproduce
verbatim (the `.AST` error text now names `WhateverCurry(...)` rather than the built closure,
which is Phase 1 working as designed).

### 2.1 The leaf rule is syntactic and scope-independent (measured)

The single most important correction to a plausible-but-wrong shortcut: **do not derive the
leaf classification from mutsu's existing "does this subtree curry" predicates.** Rakudo
decides `Term::Whatever` vs `WhateverCode::Argument` from the operand's *immediate syntactic
parent*, before and independently of any scope derivation. The two coincide almost
everywhere in mutsu, but not everywhere — `1 x *` is the counter-example: mutsu plants no
`WhateverCurry` there (`should_wrap_whatevercode` exempts `x` with a bare-`*` right operand,
`whatever.rs:54-62`), yet `(1 x *).WHAT` is `(WhateverCode)` in *both* implementations
because the currying happens elsewhere at runtime. A scope-derived classifier would render
`Term::Whatever` where raku renders `WhateverCode::Argument`.

The rule, measured leaf by leaf with `raku -e 'say Q[…].AST'`:

| source | the `*` renders as |
|---|---|
| `1, *, 2` (comma operand), `say *` / any call or method **argument** | `Term::Whatever` |
| `1..*` (bare range endpoint) — but `1..*-1` | `Term::Whatever` — but `Argument` |
| `1, 2 ... *`, `1, 2 ...^ *` (series operand) | `Term::Whatever` |
| `my $x = *`, `my $x := *` (assignment / bind RHS) | `Term::Whatever` |
| `* xx 2`, `1 xx *` | `Term::Whatever` |
| `* x 2`, `1 x *` | `Argument` (both operands) |
| `* ff *` (flip-flop operands) | `Term::Whatever` |
| `@a[*]` (whole-slice subscript) — but `@a[*-1]` | `Term::Whatever` — but `Argument` |
| `*(1)` (invoking a bare `*`) | `Term::Whatever` |
| `*.WHAT` / `.WHO` / `.HOW` / `.WHERE` / `.DEFINITE` / `.VAR` — but `*.WHICH`, `*.abs` | `Term::Whatever` — but `Argument` |
| `(a => *)` bareword key — but `("k" => *)` | `Term::Whatever` — but `Argument` |
| `(*)`, `[*]`, `{ * }`, a bare `*` statement | `Term::Whatever` |
| `* ~~ Int`, `Int ~~ *`, `$_ ~~ *`, `* !~~ Int` | `Argument` (**both** sides) |
| `-*`, `?*`, `*++`, `* Z 1`, `* X 1`, `1 R- *` | `Argument` |
| `* .= lc` | `Term::Whatever` (raku models it `ApplyDottyInfix`) |

Two properties worth stating because they make the implementation tractable:

- The `Term::Whatever` rows are **exactly** the opt-out arms mutsu already hand-wrote in
  `src/parser/expr/whatever.rs` (`contains_whatever` `:155-286`, `should_wrap_whatevercode`
  `:14-65`, `contains_xx_with_bare_whatever` `:97-136`). §1's "the predicates are not deleted
  — they are re-aimed" is therefore literally true: the classifier is those same arms,
  rewritten to answer "is this `*` a value" instead of "should I wrap this subtree".
- Every runtime behaviour implied by the table **already matches** in mutsu. Verified on
  `main`: `(1 x *).WHAT`, `(1 xx *).WHAT`, `($_ ~~ *).WHAT`, `(* ~~ Int).WHAT`,
  `(* Z 1).WHAT`, `(* X 1).WHAT`, `(**).WHAT`, `@a[*].WHAT`, `@a[*-1]` all agree with raku.
  So Phase 2 is a pure *representation* change: nothing it touches should alter a result.

### 2.2 In Phase 2, `Expr::WhateverArg` is a pure annotation

The mechanism that makes a change of this blast radius safe is the same one Phase 1 used —
be behaviour-preserving *by construction* rather than by testing:

> **Invariant.** Outside `src/rakuast/`, `Expr::WhateverArg` and `Expr::Whatever` are
> indistinguishable. Phase 2 adds no consumer that branches on which one it got.

Concretely:

- `crate::parser::is_whatever` (`whatever.rs:138`) becomes
  `matches!(expr, Expr::Whatever | Expr::WhateverArg)`. Because today every leaf is
  `Expr::Whatever`, every predicate built on it (`contains_whatever`, `count_whatever`,
  `replace_whatever_single`, `replace_whatever_numbered`, `should_wrap_whatevercode`) then
  computes exactly what it computes today, *whatever the classifier decides*. A
  mis-classified leaf becomes a wrong `.AST` gist — never a wrong program result.
- The three `matches!(&**right, Expr::Whatever)` / `matches!(&**left, Expr::Whatever)` literal
  tests in the `x`/`xx` arms (`whatever.rs:58-59`, `:110-111`) and the `is_whatever(target)`
  test in the `CallOn` arm (`:46`) must go through the same widened predicate, not stay
  variant-literal.
- The compiler's two arms (`src/compiler/expr.rs:51` and `:59`) already emit an identical
  `LoadConst(Value::WHATEVER)` and stay that way — merge them or leave them, but do not make
  `WhateverArg` compile differently. This is required, not cosmetic: §2.1's `1 x *` shows a
  `WhateverArg` leaf can legitimately sit outside any `WhateverCurry` in Phase 2 and must
  still compile to a plain Whatever value for the runtime autoprime to work. Drop the
  "not yet produced by the parser" comment above `:59` as part of the change.
- `expr_contains_topic` (`whatever_curry/build.rs:223`) already lists both variants; leave it.

Phase 4 is what *upgrades* the annotation into a load-bearing signal (`plant` deriving scopes
from marked leaves). Phase 2 must not anticipate that.

### 2.3 Where the classifier runs

A new module `src/whatever_curry/mark.rs`, exposing

```rust
pub(crate) fn mark_program(stmts: &mut Vec<Stmt>);
```

a single top-down walk that rewrites each `Expr::Whatever` to `Expr::WhateverArg` unless its
immediate parent context puts it in a §2.1 value position. It is invoked once from
`parser::parse_program` (`src/parser/mod.rs:350`), after `stmt::program` succeeds and
alongside the existing post-parse passes — `src/placeholder_order.rs` is the precedent for a
crate-root post-parse AST transform, and `parse_program` is the single choke point shared by
ordinary execution, module loads, `EVAL`, and `Str.AST` (`rakuast::str_dot_ast` reaches it
via `parse_dispatch::parse_source`, `src/rakuast/mod.rs:491`).

Why a post-parse pass rather than the parser's `*` term site: the classification needs the
*parent*, which the term parser does not have. Why not inside `rakuast::convert` (walking
with a context parameter and never materialising the variant): because Phase 3 must be able
to *produce* the leaf when lowering a hand-constructed
`RakuAST::WhateverCode::Argument.new`, and Phase 4's `plant` must be able to *read* it; a
converter-local context parameter serves neither. `mark.rs` is deliberately the seed of the
`plant.rs` this ADR's §4 calls for — same traversal, same parent-context switch, one phase
later it also decides where scopes begin.

Cost: one extra traversal of every parsed program. Implement it unconditionally and read the
bench CI (`bench-history.tsv` on the `bench-data` branch — local A/B is not the source of
truth, per CLAUDE.md). If it registers, gate the walk behind a flag the `*` term parser sets;
do not pre-optimise it into the design.

### 2.4 Converter and metadata changes

In `src/rakuast/mod.rs`:

- New `RakuAstClass::WhateverCodeArgument` (enum near `TermWhatever`, `:141`), printed
  `"RakuAST::WhateverCode::Argument"` (`:226`), added to `empty_parens_omitted` (`:238`) —
  raku's gist is a bare `RakuAST::WhateverCode::Argument.new` with no parens, measured — and
  to the `ALL_CLASSES` registry list (`:482`).
- **It must be added to the `TERM` arm of `semantic_ancestors` (`:278-298`) explicitly.**
  `TermWhatever` gets `Term`/`Expression` for free from the `RakuAST::Term::` name-prefix
  rule in `type_object_isa` (`:305-330`); `RakuAST::WhateverCode::Argument` does not start
  with that prefix, so without the explicit entry `$node ~~ RakuAST::Term` is silently false.
  Rakudo's MRO, measured: `Argument, Term, Termish, Expression, …, Node` — a Term and an
  Expression, same as the other leaf terms.
- Bonus, in the same edit: `RakuAstClass::TermHyperWhatever` →
  `"RakuAST::Term::HyperWhatever"` for `Expr::HyperWhatever` (`**`), which is a one-line
  converter arm and today an outright `.AST` failure. Its MRO is the same `Term`/`Expression`
  shape (measured), and the `RakuAST::Term::` prefix covers it. This ADR's §1 excludes `**`
  from the *priming* work; it does not exclude giving it a read-direction node.

In `src/rakuast/convert.rs`:

- `Expr::WhateverArg` → `WhateverCodeArgument` (next to the `Expr::Whatever` arm at `:828`).
- `Expr::WhateverCurry(body)` → `convert_expr(body)`, **no wrapper node** (§5): Rakudo's tree
  carries no priming scope, and `(* + 1) * 2` gets its `Circumfix::Parentheses` from mutsu's
  existing `Expr::Grouped` arm (slice 21), not from the curry marker.
- The two `is_whatever_code` guards at `:1107` and `:1120` are only removable once §2.5 has
  eliminated the eager sites that still reach them; until then they stay, and their error
  text should name the surviving construct rather than "Whatever-code closure".

### 2.5 The remaining eager construction sites are part of this phase

Phase 1's Outcome deliberately left three hand-rolled autoprime paths building a closure
directly instead of planting a marker. Measured on `main`, they are exactly the forms whose
`.AST` still fails with `Whatever-code closure`: `* ~~ Int`, `Int ~~ *`, `$_ ~~ *`,
`* !~~ Int` (`src/parser/expr/precedence/comparison.rs`, and `wrap_smartmatch_rhs` in
`precedence/chain_cmp.rs:83-111`), and `* += 1` / `* -= 2`
(`precedence/assign.rs`, `stmt/assign/compound_expr.rs`).

Phase 2 converts the **smartmatch** family to `Expr::WhateverCurry` — they need no new
RakuAST class (the `Int` RHS is the existing `Type::Simple`, slice 26) and §2.1 shows raku
marks both operands `Argument`, so they render correctly the moment the marker replaces the
closure. Keep the runtime behaviour pinned: `(* ~~ Int).WHAT`, `(Int ~~ *).WHAT` and
`($_ ~~ *).WHAT` are all `(WhateverCode)` today and must stay so; note that
`contains_whatever` / `count_whatever` / `replace_whatever_*` all special-case
`SmartMatch`/`BangTilde` to look at the **left** operand only, so routing an `Int ~~ *`
through `build_closure` needs that asymmetry re-checked rather than assumed.

The **compound-assignment** family stays a boundary in Phase 2: raku models `* += 1` as
`ApplyInfix(…, infix => RakuAST::MetaInfix::Assign.new(RakuAST::Infix.new("+")), …)`, and
mutsu has no `MetaInfix::Assign` class. Adding one is an independent read-direction slice
(it is not Whatever-specific — every `$x += 1` has the same gap), so it belongs with the
operator cluster, not here.

### 2.6 Divergences Phase 2 documents rather than closes

- **Chained comparison.** raku renders `1 < * < 10` as a left-nested
  `ApplyInfix(ApplyInfix(1, "<", Argument), "<", 10)`. mutsu has no chained-comparison node:
  `chain_cmp.rs` either duplicates a pure middle operand into `(a < m) && (m < b)`
  (`build_chain_cmp_expr_with_repeated_middle`, `:59-81`) or, for an effectful middle, emits
  a `DoBlock` with a `__mutsu_chain_cmp_N` temporary (`:19-57`) — so `Q[1 < 2 < 3].AST`
  already renders `RakuAST::StatementPrefix::Do` today, an *existing* divergence that is not
  about Whatever at all. Runtime semantics are correct in both shapes (measured: the middle
  is evaluated exactly once, and `(1 < * < 10).arity` is 1 in both implementations).
  Record it in the same register as ADR-0011's `unless` → `if !` note. The `Expr::ChainedCompare`
  node this ADR's "Phase-4 prerequisite" section calls for closes it for both phases at once;
  it is not a Phase-2 dependency.
- **`* .= lc`** already converts, but as mutsu's own shape rather than raku's
  `ApplyDottyInfix` / `DottyInfix::CallAssign` — the pre-existing `.=` gap already listed
  under "Read-direction representation gaps" in `todo/deep/rakuast-remaining.md`.

### 2.7 Test plan

`t/rakuast-whatever-code.t`, dual-oracle (must pass under **both** mutsu and raku, the
ADR-0011 convention). Assert on `.AST.gist` for the shapes and on `.^name` / `~~` for the
hierarchy:

- Argument leaves: `* + 1`, `* + *`, `*.abs`, `*.WHICH`, `1..*-1`, `(* + 1) * 2`, `@a[* - 1]`,
  `-*`, `?*`, `*++`, `* x 2`, `1 x *`, `* ~~ Int`, `Int ~~ *`, `$_ ~~ *`, `* !~~ Int`,
  `"k" => *`, `(1, 2).map(* + 1)`, `(* - 1) o (* * 2)`.
- Value leaves (regression guard for over-marking): `1, *, 2`, `1..*`, `1, 2 ... *`,
  `my $x = *`, `* xx 2`, `1 xx *`, `@a[*]`, `*(1)`, `*.WHAT`, `(a => *)`, `say *`, `(*)`,
  `[*]`.
- Hierarchy: the `* + 1` left operand `.^name` is `RakuAST::WhateverCode::Argument`, and it
  `~~ RakuAST::Term`, `~~ RakuAST::Expression`, `~~ RakuAST::Node`.
- `**` → `RakuAST::Term::HyperWhatever`.

Plus a runtime no-change guard: the existing `t/*whatever*.t` (35 files),
`roast/S02-types/{whatever,hyperwhatever}.t` and `roast/S03-operators/composition.t` must be
green unchanged — they are the same surface Phase 1 leaned on.

### 2.8 Risks specific to Phase 2

- **Over-marking is invisible at runtime.** The §2.2 invariant is what buys the safety, and
  it is also what hides a classifier bug: a leaf marked wrongly changes only the `.AST` gist.
  The value-leaf half of the §2.7 test list is therefore not optional padding — it is the
  only detector.
- **`is_whatever` widening must be total.** The three variant-literal `matches!` tests called
  out in §2.2 are the ones a `grep` for `is_whatever` misses. Missing one turns a `xx` or
  `CallOn` opt-out off and *does* change results.
- **Smartmatch re-routing (§2.5) is the one part that can regress**, because it replaces a
  hand-built closure with the generic `build_closure` path under operand-asymmetric
  predicates. If it proves awkward, splitting it into its own follow-up PR is legitimate;
  splitting the *leaf classification* is not, since nothing else in Phase 2 is useful without
  it.
- **File-size cap.** `src/rakuast/convert.rs` (1795 lines) and `mod.rs` (1048) are already
  over the repo's 500-line convention. Per the Risks section above, land the per-cluster
  split *with* this phase rather than growing them further.

## References

- [ADR-0011](0011-rakuast-model-layer-and-phasing.md) — the parent decision; Phase 5's
  "Larger machinery" boundary names `WhateverCode` explicitly.
- [`todo/deep/rakuast-remaining.md`](../../todo/deep/rakuast-remaining.md) — the live
  inventory this ADR removes one item from.
- `raku-doc/doc/Type/Whatever.rakudoc:15-88` — Whatever-priming and its exception table.
- `src/parser/expr/whatever.rs`, `whatever_wrap.rs`, `whatever_replace.rs` — the current
  parser-side implementation.
- `src/rakuast/convert.rs:828`, `:1100-1123`; `src/rakuast/lower.rs:655`;
  `src/rakuast/mod.rs:141,226,239` — the RakuAST sites this ADR changes.
