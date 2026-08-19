# ADR-0033: Whatever-priming is a leaf property plus a derived scope — defer `WhateverCode` construction out of the parser

- **Status**: Phase 1 shipped (2026-08-19). Phases 2-4 not started — see "Outcome" below.
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
