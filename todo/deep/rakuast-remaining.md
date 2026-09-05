# RakuAST remaining work

RakuAST is a reflection/model layer bidirectionally convertible with mutsu's
internal `Expr`/`Stmt` AST, not a frontend rewrite. The fixed design and phasing
are in [ADR-0011](../../docs/adr/0011-rakuast-model-layer-and-phasing.md).
Completed read, introspection, construction, and EVAL slices are recorded in
[the July 2026 news](../../news/2026-07.md) and the individual
`news/2026-07/rakuast-*.md` entries; the 2026-08 return-type / hyper-infix slice
is [here](../../news/2026-08/rakuast-return-types-and-hyper-infix.md).

## Current parity status

RakuAST is **not yet a general `raku`-equivalence oracle**. The existing
`t/rakuast*.t` files are dual-oracle tests for the constructs that mutsu has
implemented so far; they do not imply that an arbitrary source program has the
same `.AST` shape or that every construct can be lowered through `EVAL`. As of
2026-09-02, the suite covered 93 files and 646 assertions.

## Campaign organization

This file is the campaign overview, not a fourth `todo/rakuast/` category.
Self-contained slices belong in `todo/tickets/rakuast-<slug>.md`; parser,
internal-AST, or execution redesigns belong in `todo/deep/rakuast-<slug>.md`.
Stable workflow guidance is in [`docs/rakuast/README.md`](../../docs/rakuast/README.md),
and the reusable implementation procedure is in
[`.agents/skills/rakuast-implementation/SKILL.md`](../../.agents/skills/rakuast-implementation/SKILL.md).
Each slice should close both RakuAST directions and add focused dual-oracle
coverage before it is marked complete.

The 2026-09-02 direct probes against the system `raku` established the current
boundary:

- Ordinary scalar compound assignment (`$x += 3`), indexed compound assignment
  (`@a[0] += 3`), `AT-POS` compound assignment, and defined-or assignment
  (`$x //= 3`) now render the same `ApplyInfix` shape with
  `MetaInfix::Assign(Infix(...))` in both implementations. Their execution
  through `EVAL` also agrees for the covered cases.
- Mutating method assignment (`$x .= abs`) still differs: `raku` retains
  `ApplyDottyInfix` with `DottyInfix::CallAssign`, while mutsu still exposes its
  lowered method-call assignment form. This remains an open representation
  boundary below.
- Therefore, “RakuAST is basically equivalent to `raku`” is currently true only
  for the implemented and pinned slices, not for the RakuAST surface as a whole.

## Read-direction representation gaps

Several source constructs are desugared or lose distinctions before the RakuAST
conversion sees them. Recovering these requires preserving the distinction in
the parser/internal AST, not guessing during RakuAST conversion.

**Closed 2026-08-22** (both directions, pinned by `t/rakuast-return-type.t` and
`t/rakuast-hyper-infix.t`):

- *Signature return types on `sub` and pointy blocks.* `sub f(--> Int)` now
  renders `Signature.returns`, `sub f() returns Int` renders
  `traits => (Trait::Returns(...),)`, and `sub f() of Int` renders
  `Trait::Of` — mutsu's internal AST already distinguished the two spellings
  (a `__return_via_trait` marker in `custom_traits`), and the `of` form now
  carries its own `__return_via_of` marker so the third node choice is not a
  guess either. `EVAL` lowers all three back. A parameter-less signature
  renders its empty parameter list as raku's `$( )` (mutsu printed a bare
  multi-line `(\n)` before). Fixing the pointy-block case also fixed a real
  runtime bug: a *single*-parameter pointy block parsed to `Expr::Lambda`,
  which has nowhere to keep a return type, so `-> $x --> Int { "s" }`
  silently returned a `Str` (pinned by `t/pointy-block-return-type.t`).
- *Hyper infix operators.* `@a >>+<< @b` renders
  `ApplyInfix(left, MetaInfix::Hyper([dwim-left,] infix, [dwim-right]), right)`
  and lowers back. mutsu's `Expr::HyperOp` already kept the operator text and
  both dwim flags, so this is a 1:1 mapping.

**Closed 2026-09-03** (both directions, pinned by
`t/rakuast-hyper-function-infix.t`):

- *Hyper function infix operators.* `@a >>[&infix:<+>]<< @b` now renders
  `MetaInfix::Hyper(FunctionInfix(Var::Lexical))` with the measured DWIM fields,
  and lowers back through the existing `Expr::HyperFuncOp` execution path.

**Closed 2026-09-05**:

- *A bareword naming something the unit declared, and `.^name`.* A declared
  type renders as `Type::Simple` and a declared constant as `Term::Name`, so a
  program can finally use a class or constant it declares — which is what forced
  the earlier declaration slices to inspect the `EVAL`'d value from outside.
  `.^name` also moved from a `Call::Method` with a `.^` dispatch field to raku's
  own `Call::MetaMethod`. Pinned by `t/rakuast-declared-name.t`; see
  [the news entry](../../news/2026-09/rakuast-declared-name-and-metamethod.md).
- *Class traits, `multi`, and `constant`.* `class C is P`, `class C does R`,
  `class C is rw`, `is repr(...)`, `multi sub`, and `constant X = 5` all render
  (byte-identical to rakudo 2026.07) and lower back. Pinned by
  `t/rakuast-class-traits-multi-constant.t`; see
  [the news entry](../../news/2026-09/rakuast-class-traits-multi-constant.md).
- *Phasers.* `BEGIN` / `CHECK` / `INIT` / `END` / `ENTER` / `LEAVE` / `KEEP` /
  `UNDO` / `FIRST` / `NEXT` / `LAST` / `QUIT` / `CLOSE` render as
  `RakuAST::StatementPrefix::Phaser::<Kind>` (byte-identical to rakudo 2026.07)
  and all but `BEGIN` lower back. `PRE`/`POST` stay a boundary on both sides
  (rakudo wraps their block in a call, and mutsu keeps a source-text condition).
  Fixing the write direction also made the `EVAL` carrier apply
  `reorder_phasers_for_eval`, so `INIT`/`CHECK` run before the mainline as they
  must; `BEGIN` needs a compile-time hoist the carrier lacks and is refused —
  `todo/tickets/rakuast-eval-begin-phaser.md`. Pinned by `t/rakuast-phaser.t`;
  see [the news entry](../../news/2026-09/rakuast-phasers.md).
- *Where-constrained parameters (read direction).* `sub f($x where * > 0)`
  renders its `where` field — the shape `RakuAST::Parameter.new(:where)` already
  built and `EVAL` already lowered and enforced. Fixing it also made the
  ADR-0033 leaf classifier walk a routine's `param_defs`, so a `*` in a
  signature is a `WhateverCode::Argument` rather than a `Term::Whatever`. Pinned
  by `t/rakuast-where-parameter.t`; see
  [the news entry](../../news/2026-09/rakuast-where-constrained-parameter.md).
- *`andthen` / `orelse` / `notandthen` lowering.* raku models these as list
  infixes, so they render as `ApplyListInfix` — the node a comma list uses — and
  only `,` lowered. They now fold back into mutsu's left-nested `Expr::Binary`
  shape. Pinned by `t/rakuast-eval-andthen.t`; see
  [the news entry](../../news/2026-09/rakuast-andthen-family-lowering.md).
- *Reduction and arity-0 pointy-block lowering.* `EVAL` lowers
  `RakuAST::Term::Reduce` (`[+] @a` and the triangle `[\+] @a`, whose marker
  mutsu keeps inside the operator string) and a zero-parameter
  `RakuAST::PointyBlock` (`-> { … }`, the same `AnonSubParams` node the parser
  builds, so the lowered closure keeps arity 0). Pinned by
  `t/rakuast-eval-reduce-pointy.t`; see
  [the news entry](../../news/2026-09/rakuast-reduce-and-arity-0-pointy-lowering.md).
- *Postfix lowering.* `EVAL` lowers the rest of the `ApplyPostfix` cluster:
  `Postfix` (`$x++`/`$x--`), `MetaPostfix::Hyper` (`@a>>.abs`), a
  `Call::Method`'s `.?`/`.+`/`.*` dispatch modifier, and `Call::QuotedMethod`.
  It also fixed a silent no-op: `op_name_to_token_kind` had no `++`/`--` row, so
  the already-lowering *prefix* `++$x` became `Unary { op: Ident("++") }` and did
  not increment. Pinned by `t/rakuast-eval-postfix.t`; see
  [the news entry](../../news/2026-09/rakuast-postfix-lowering.md).
- *Class / role / method / attribute lowering.* `EVAL` accepts
  `RakuAST::Class`, `RakuAST::Role`, `RakuAST::Method`, and the attribute form
  of `VarDeclaration::Simple` (`scope => "has"`), lowering them to
  `Stmt::ClassDecl` / `RoleDecl` / `MethodDecl` / `HasDecl`. The declaration
  cluster had been readable since Phase 2 slice 13 but not round-trippable.
  Pinned by `t/rakuast-eval-class.t`; see
  [the news entry](../../news/2026-09/rakuast-class-role-method-lowering.md).
- *Anonymous subs with explicit signatures.* `sub ($x) { }` renders as a
  nameless `RakuAST::Sub` (its parameters carrying the implicit
  `Type::Setting(Any)` a pointy block's do not) and lowers back through `EVAL`;
  `-> $a, $b { }` still renders as a `PointyBlock`. `Expr::AnonSubParams`
  carries an `is_sub` flag set only by the `sub` declarator; it has no
  execution meaning. Pinned by `t/rakuast-anon-sub-signature.t`; see
  [the news entry](../../news/2026-09/rakuast-anonymous-sub-signature.md). The
  same change made `EVAL` accept a nameless `RakuAST::Sub` at all, which it
  previously refused.
- *Signature return types on methods.* `method m(--> Int)`,
  `method m() returns Int`, and `method m() of Int` render the same three node
  shapes a `sub` does. The parser now keeps its `__return_via_*` spelling
  markers in `MethodDecl.custom_traits` (method trait application skips
  `__`-prefixed entries), so the converter reads the spelling instead of
  guessing. Pinned by `t/rakuast-method-return-type.t`; see
  [the news entry](../../news/2026-09/rakuast-method-return-types.md). The write
  direction is unaffected: `RakuAST::Method` has no `EVAL` lowering yet at all,
  which is its own slice.

Still open:

- `.=` and Whatever compound autoprime. `$x .= Str` desugars to a plain `=` over
  a method call (raku: `ApplyDottyInfix` + `DottyInfix::CallAssign`) and still
  needs the parser to retain its dotty-infix form. Core compound assignment is
  now closed: `+=`, `-=`, `*=`, `~=`, `//=`, `||=`, `&&=`, indexed lvalues, and
  `AT-POS` lvalues preserve `MetaInfix::Assign(Infix(...))` for `.AST`, while
  `EVAL` reuses the existing execution expansion. The `* += 1` Whatever
  autoprime path remains a separate boundary because it still builds its
  closure before conversion; ADR-0033 §2.5 tracks that case.
- The remaining hyper forms: hyper *prefix* (`-<<@a`, desugared to a
  `__mutsu_hyper_prefix` call), hyper *postcircumfix* (`@a>>[1]`, desugared to a
  hyper `AT-POS` method call), and `@a<<.abs` (which mutsu's parser currently
  reads as a quote-words subscript). Since 2026-09-05 the desugared *call* forms
  are an explicit `.AST` boundary rather than a node naming a mutsu internal —
  see [the news entry](../../news/2026-09/rakuast-desugar-marker-boundary.md).
  The `@a>>[1]` case still renders a hyper `AT-POS` `Call::Method`, which is a
  wrong node rather than an internal name, so it needs its own measured slice.
- `with` / `without`. Desugared at parse time into a `__with_tmp_N` temp var plus
  an `if` on `.defined`, so there is no statement to map to
  `Statement::With` / `Statement::Without`. It is an explicit boundary (the temp
  var's internal name is refused), not a wrong rendering.
- Grammar declarations. `grammar G { }` is a `Stmt::ClassDecl` with
  `parents = ["Grammar"]`; class inheritance itself is closed now, so what
  remains is producing `RakuAST::Grammar` + `TokenDeclaration` + the regex node
  tree rather than a `Class` with a `Grammar` parent.
- Associative `%h{...}` versus `%h<...>` subscripts. Both are
  `Expr::Index { is_positional: false }` with a `Literal(Str)` index, so the read
  side cannot choose raku's `Postcircumfix::LiteralHashIndex` (a word-quoted
  `QuotedString`) over `Postcircumfix::HashIndex` (a `SemiList`). This is the
  single highest-impact remaining read gap — associative subscripts also block
  the corresponding `EVAL` item below — but it needs a third state on
  `Expr::Index` (or a `SubscriptKind` enum replacing `is_positional`), and that
  field is touched at ~225 sites across the parser, compiler, and VM.

## Type-object introspection

The RakuAST type registry supports the common hierarchy, method, attribute, and
`.^can` operations. Audit and implement the remaining metaobject operations from
the same model metadata. Do not expose Rakudo compiler-private `IMPL-*` details.

**Closed 2026-09-05:** `.^methods` (with no adverb) and `.^lookup` answered `()`
and `(Mu)` for a method `.^methods(:local)`, `.^can` and `.^method_table` could
all see. All four now read the model metadata through one MRO-walking helper,
with the three `.^methods` adverb cases following
`Type/Metamodel/MethodContainer.rakudoc`. Pinned by `t/rakuast-type-lookup.t`;
see [the news entry](../../news/2026-09/rakuast-type-object-lookup-and-methods.md).

## Construction

Advanced parameter construction remains:

- sub-signatures
- type captures
- array shapes

Signature constraints (`where`) are closed as of 2026-09-05: `.new(:where)`
already built the node and `EVAL` already enforced it, and the read direction
now renders it too.

These must validate, render, expose through introspection, and lower through
`EVAL` consistently with the already-supported parameter forms.

`RakuAST::Signature.new` accepts no `returns` argument yet, and
`RakuAST::Trait::Returns` / `Trait::Of` are read-and-lower-only (the converter
builds them, `EVAL` lowers them, but there is no `.new` constructor). **Closed
2026-09-02:** `Signature.new(:returns)`, `Trait::Returns.new($type)`, and
`Trait::Of.new($type)` now construct the existing node shapes; `Sub.new(:traits)`
can attach the return traits so hand-built nodes reach the existing lowerer.
Regression coverage is in `t/rakuast-construct-return-type.t`.

## Lowering and EVAL

The remaining constructs are blocked by representation mismatches and each needs
an explicit design:

- placeholder blocks such as `{ $^a }`
- `with` / `without`
- list assignment
- `constant`
- associative subscripts
- `CATCH` blocks
- code-block interpolation
- regexes

Pick these deliberately by user impact rather than treating them as another
cadence of mechanical slices. Lower through the existing internal AST and
compiler; do not add a second execution engine.

### WhateverCode (ADR-0033) — all four phases shipped

`* + 1` was picked first because it is the highest-frequency construct on the list
(`.map(* + 1)`, `.grep(* > 3)`, `@a[* - 1]`) and because investigating it surfaced a
second, independent defect that shares the same root cause: mutsu primes straight
through the thunky operators, so `(* > 3 && * < 8).arity` is `2` where raku says `1`,
`(1..10).grep(* > 3 && * < 8)` returns `5 6` where raku returns `1..7`, and a ternary
primes nothing at all. Both follow from mutsu building the `WhateverCode` closure
eagerly in the parser at ~50 call sites, which destroys the pre-curry expression before
the RakuAST converter can see it and leaves no single owner for the priming-scope rule.

[ADR-0033](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md) proposes
Rakudo's model — a leaf split (`Expr::Whatever` value vs `Expr::WhateverArg` argument),
an `Expr::WhateverCurry` scope marker, closure construction moved to the compiler, and a
single `whatever_curry::plant` shared by the parser and `rakuast::lower` — in four
phases, the first of which is behaviour-preserving.

Phase 1 (the behaviour-preserving deferral) shipped 2026-08-19: `src/whatever_curry/`
now owns closure construction, invoked from a single `Expr::WhateverCurry` compiler arm;
the parser's ~50 `wrap_whatevercode` call sites construct that marker instead of building
the closure eagerly, verified zero-behaviour-change against the full `t/` + targeted
roast suites. See the ADR's own Outcome section for the full list of latent-bug fixes
this deferral surfaced along the way.

Phase 2 — the *leaf* half (`Expr::WhateverArg` → `RakuAST::WhateverCode::Argument`) —
shipped 2026-08-20 (same day as its design): `src/whatever_curry/mark.rs` classifies
every `*` leaf post-parse per the ADR's raku-measured table, `Q[* + 1].AST` now renders
correctly, and the change was verified behaviour-preserving by construction (`is_whatever`
treats both leaf variants identically everywhere outside `src/rakuast/`) plus a new
dual-oracle `t/rakuast-whatever-code.t` (68 assertions, passes verbatim under mutsu and
the system raku). See the ADR's "Phase 2 outcome" section for the full change list,
including two adjacent RakuAST operator-name rendering bugs (`!~~`, `=>`) it fixed along
the way and one latent runtime bug it fixed as a side effect (`$_ ~~ *` previously
shadowed the caller's topic instead of reading it dynamically).

Phase 4 — the thunk-barrier priming correctness fix — shipped 2026-08-23. It was a
genuine, user-visible correctness bug independent of RakuAST: `(1..10).grep(* > 3 && * < 8)`
silently returned `5 6` instead of raku's `1 2 3 4 5 6 7`, because mutsu primed straight
through the thunky operators and built one arity-2 closure where raku builds two
independent arity-1 ones; a ternary primed nothing at all. `src/whatever_curry/plant.rs`
now owns the barrier rule (`&&`/`||`/`//`/`and`/`or`/`andthen`/`orelse`/`notandthen`/
ternary), and the barrier is *opaque* to the enclosing scope, which is what let the ~50
parser planting sites fall into line without being rewritten. The ADR's "Phase-4
prerequisite" (the chained-comparison `&&`-duplication trap) was first resolved with a
dedicated `TokenKind::ChainAnd`; the heavier `Expr::ChainedCompare` node it deferred (for
RakuAST rendering fidelity) shipped 2026-08-26 and retired `ChainAnd` in the process — see
[`news/2026-08/chained-compare-ast-node.md`](../../news/2026-08/chained-compare-ast-node.md).
See the ADR's "Phase 4 outcome" section for the full account of the original prerequisite,
including the `xor` / `^^` re-measurement and a latent de-duplication bug it flushed out.

Phase 3 — the RakuAST write direction — shipped 2026-09-05, completing the ADR.
`RakuAST::WhateverCode::Argument` lowers to `Expr::WhateverArg`, and the priming *scope*
a lowered tree has no parser to plant is now derived by the same authority: the
`whatever_curry::mark` walk runs in a mode that materialises a scope at the first
(outermost) expression that primes, which is what makes the scope maximal. Two
corrections came out of differential testing rather than reading — a marker's own body
must not be re-planted (it recursed until the stack overflowed), and an invocation is
never itself a scope (`(* + 1)(4)` evaluated to the closure instead of calling it). The
oracle is mutsu against itself: running `S` must equal `EVAL(Q{S}.AST)`. Pinned by
`t/rakuast-eval-whatever-code.t`; see
[the news entry](../../news/2026-09/rakuast-whatever-code-eval.md) and the ADR's
"Phase 3 outcome" section.

The remaining items on both lists above are still undesigned.

## Macros

Macros, `quasi`, and unquoting depend on the construction and lowering layers.
They remain a separate future campaign and may be deferred indefinitely until a
real use case justifies it.
