# ADR-0076: One bare-block lowering, two block-scope opcodes

- **Status**: Accepted (the shared lowering landed; the opcode merge is deliberately deferred — see §6)
- **Date**: 2026-09-08
- **Related**: `src/compiler/control_block.rs`, `src/compiler/control_block_scope.rs`,
  `src/compiler/helpers_do_expr.rs`, `src/compiler/stmt.rs`,
  `src/vm/vm_misc_scope.rs` (`exec_block_scope_op`), `src/vm/vm_misc_block.rs`
  (`exec_do_block_expr_op`, `exec_let_block_op`),
  [ADR-0048](0048-placeholder-scope-is-a-block-invocation-contract.md) D3/D6 (placeholder attribution),
  [#7569](https://github.com/tokuhirom/mutsu/issues/7569),
  `news/2026-09/unify-statement-expression-control-construct-compilation.md`

## 1. Context

`for` and `if`/`elsif` were unified into one lowering each (`src/compiler/control_for.rs`,
`src/compiler/control_if.rs`), shared by both source positions and parameterised by a
position enum. The bare `{ ... }` block was the one construct left with two independent
compilation passes:

- **Statement position** — the `Stmt::Block` arm of `Compiler::compile_stmt`.
- **Value position** — `compile_do_block_expr` / `compile_do_block_expr_scoped`
  (`do { ... }`, a block used as a term, the parser's labelled-block lowering).

Both had to answer the same questions about the body — does it declare routines? does it
import? does it need a per-execution `ResetStateLocals`? does a `CATCH`/`CONTROL` in it
make it an implicit `try`? do `ENTER`/`LEAVE`/`KEEP`/`UNDO` make it a phaser block scope?
— and each wrote its own answer. #7569 recorded this as the residue of the
statement/expression unification campaign.

The reason it did not fall out of the `for`/`if` work is stated in the issue: those two
shared a single opcode and a single skeleton, and the block pair shares **neither**. The
statement form emits `OpCode::BlockScope`; the value form emits `OpCode::DoBlockExpr`.

The split had drifted, and measurably so. Against `raku` as the oracle, the value copy was
missing three decisions the statement copy makes:

| Probe | `raku` | mutsu before |
| --- | --- | --- |
| `our $g = 1; do { temp $g = 2; 1 }; say $g` | `1` | `2` — no `LetBlock`, so `temp` was never restored |
| `do { my \str = "hi"; 1 }; say str.^name` | `str` | `Str` — the sigilless shadow of the native type name outlived the block |
| `{ use MONKEY-SEE-NO-EVAL; my $y = 42 }; say $::('y')` | `(Any)` | `42` — a *statement* block that imports got no block scope at all |

Neither of the first two is a bug in `OpCode::DoBlockExpr`: each is a decision the value
pass simply never grew, because it was a second pass that nobody thought to update. The
third runs the other way — see §5 — and is the statement pass's own drift.

## 2. Decision

**Split the question in two, and answer them differently.**

1. **The lowering is unified.** `BlockPlan::analyze` (`src/compiler/control_block.rs`) is
   the single place every question above is answered, and
   `Compiler::compile_block_construct` the single skeleton that consumes the answer.
   `BlockPosition` (`Statement` / `Value { isolate }`) names everything that genuinely
   differs. Both former entry points are now thin wrappers over it.

2. **The two opcodes stay.** `OpCode::BlockScope` and `OpCode::DoBlockExpr` are *not*
   merged, and the statement form is *not* expressed as the value form with the result
   discarded. §4 says what each guarantees; §5 and §6 say why merging them is a separate
   decision that this ADR deliberately does not take.

## 3. What the shared plan decides

`BlockPlan` carries five fields, and `BlockShape` is the exclusive one:

- `shape`: `ImplicitTry` (a `CATCH`/`CONTROL`) → `PhaserScope` (`ENTER`/`LEAVE`/`KEEP`/`UNDO`)
  → `LetScope` (`let`/`temp`) → `Plain`. The test order is load-bearing and is now written
  down once.
- `import_scope`: a `use`/`no`/`import` directly in the body. **Orthogonal to `shape` in
  both positions** — see §5.
- `owns_let_scope` (an argument, not a field): false for a `DoBlock` the parser
  synthesized for something that is not a source-level block — see §5.1.
- `declares_routines`: feeds `OpCode::DoBlockExpr`'s `scope_routines` (`BlockScope` always
  snapshots the routine registry).
- `let_needs_value`: a real `let` rather than a bare `temp`, so the block's value has to
  reach the topic register `exec_let_block_op` reads.
- `succeed_barrier`: statement position only. The value form catches `succeed` inside
  `OpCode::DoBlockExpr` itself, because it has to *push* the escaping value; the statement
  form needs the standalone `OpCode::SucceedBarrier`, which truncates instead.

Position-specific pieces that stay position-specific, and why:

- **Placeholder attribution** (ADR-0048 D3/D6). A bare `{ ... }` statement is a block raku
  invokes with zero arguments, so `{ $^c }` is an arity failure at invocation; a `do { }`
  takes no signature at all, so `$^c` in it is `X::Placeholder::Block` at compile time.
  These are different raku errors, not two spellings of one rule. They live in
  `emit_statement_block_placeholder_gate` / `emit_value_block_placeholder_gate`.
- **The lexical scope frame.** `OUTER::` counts scope frames (`compiler/lex_scope.rs`), so
  exactly one may be pushed per block. The statement position pushes it in
  `compile_block_construct`; the value position's is pushed by the body emitter it
  delegates to (`compile_block_inline`, `compile_phaser_block_scope`). Pushing a second
  one in the shared skeleton adds a spurious level to every `$OUTER::x` inside a
  `do { ... }` — this was caught by `t/outer-var.t`-shaped probes while implementing this
  ADR, and is the reason the push is not simply hoisted into the shared code.

## 4. The two opcodes' contracts

Written down here because #7569 asked for exactly this, and because the merge question in
§6 cannot be reopened without them.

### `OpCode::BlockScope` (statement position)

- **Value stack**: fully transparent. It never pushes, pops or truncates. The body is
  compiled as statements, so nothing is left behind; but the opcode itself imposes nothing,
  which is what makes §6 possible at all.
- **env / locals**: a full lexical scope. Snapshots `env` on entry and rebuilds it on exit
  with an explicit propagation policy (package-qualified names and `Package` type objects
  propagate; `my`-declared names, a bound `$_`, and freshly declared `my $*dyn` do not).
  Slot restore is targeted under shadow slots. Tracks `block_declared_vars`,
  `outer_scope_locals` (for `OUTER::`), the lexical class scope, the enum scope and the
  once scope.
- **Callframe**: pushes an anonymous Raku callframe when `is_bare_block`, so a backtrace
  captured inside the block shows it.
- **Phaser queues**: owns five sections — PRE, ENTER, body, KEEP/UNDO, POST — plus the
  ENTER-result stack and the `should_run_success_queue` verdict.
- **Control flow**: catches nothing. `leave`, `succeed`, `last`/`next` all pass through.

### `OpCode::DoBlockExpr` (value position)

- **Value stack**: produces exactly one value. On a caught `leave`/`succeed` it truncates
  to its own base and pushes the escaping value.
- **env / locals**: nothing, unless `scope_isolate` is set — and that is a *different*
  mechanism from `BlockScope`'s, not a weaker version of it: a keep/revert heuristic driven
  by a compile-time list of the block's own scalar/array declarations, deliberately letting
  new hashes and outer-variable mutations survive (the `:into(my %h := :{})` idiom).
- **Callframe**: none.
- **Phaser queues**: none of its own. A phaser-bearing `do { }` nests
  `compile_phaser_block_scope` *inside* the `DoBlockExpr`.
- **Control flow**: catches `leave` matching its own label, and `succeed` unconditionally
  (resetting `when_matched`).

### `OpCode::LetBlock` (both positions, `LetScope`)

Value-transparent like `BlockScope`, which is why the value position can now simply wrap
its `DoBlockExpr` in one. Its only stack interaction is indirect: for a real `let` it reads
the success verdict off the topic register, so the value form emits `Dup` + `SetTopic`
where the statement form routes its last statement through `compile_last_stmt_as_topic`.

## 5. `import_scope` is orthogonal in both positions

Before this change the statement pass treated `use` as a *shape*: a `{ use Foo; ... }`
statement got `PushImportScope` + the raw statements + `PopImportScope` and **no
`BlockScope` at all**, so its `my` declarations, its typed declarations and its `sub`
hoisting all silently lost the block scope that the identical block without the `use`
gets. The value pass already treated it orthogonally (`PushImportScope` around the
`DoBlockExpr`).

The unified skeleton adopts the value pass's structure for both. This is a behaviour change
for the statement form, and an intended one: a block that imports is still a block.

It also surfaced a pre-existing bug that the old shape had been hiding. `$!` is implicitly
declared in every Raku scope, and a `try`/`CATCH` in a nested block assigns the one the
enclosing scope sees (`{ try die "b" }; say $!` prints `b` in rakudo). mutsu only creates
the `!` env key when something writes it, so `exec_block_scope_op`'s restore dropped it as
a block-local declaration whenever no *earlier* statement had already created it —
i.e. `$!` propagated out of a block only by accident. `roast/integration/error-reporting.t`
("Backtrace does not change on additional .backtrace") was passing on exactly that
accident, via the `use`-shaped block above. `$!` now propagates unconditionally.

Making `$!` propagate in turn exposed a second one, in the same causal chain: a `try` whose
control signal a `CONTROL` `when`/`default` MATCHED did not count as a normal completion,
so it left `$!` at whatever the region had inherited instead of resetting it the way the
plain success path does. `try die "stale"; try { CONTROL { default { } }; next }` then
reported the *stale* error; raku says `Any`. `t/implicit-catch-wrapper-does-not-trap.t` had
been passing on the mirror-image accident — `$!` never propagating out of the earlier block.

### 5.1 `$( stmt; ... )` is a contextualizer, not a block

`$( ... )` carries a statement list, so the parser lowers it to `Expr::DoBlock`. It is not
a Raku block: roast's `S04-blocks-and-statements/let.t` and `temp.t` pin that
`{ $(let $a = 23; $a); Mu }` restores `$a` at the *enclosing* block's exit, not at the
closing paren. Giving the value position a `LetScope` therefore needed the two told apart,
and `Expr::DoBlock` has 40-odd construction sites — so the lowering marks itself with the
sentinel label `crate::ast::STMT_LIST_CONTEXTUALIZER_LABEL`, following the existing
`__mutsu_check_phaser__` precedent. `compile_block_construct` consumes it (it never reaches
`OpCode::DoBlockExpr`'s `leave LABEL` matching) and passes `owns_let_scope: false`.

A dedicated AST node would be better than a sentinel, and is the obvious cleanup if a
second such distinction is ever needed.

## 6. Why the opcodes are not merged (rejected alternative)

The tempting move — since `BlockScope` is value-transparent — is to compile the value
position as `BlockScope` with `compile_block_inline` as the body, delete `DoBlockExpr`, and
have one opcode. It was considered and rejected **for now**, on three grounds:

1. **It is a semantic change, not a refactor.** `BlockScope` restores `env`; `DoBlockExpr`
   does not. Adopting it would make `do { my $w = 9; 1 }` stop leaking `$w` — which is what
   raku does, and is a real bug (`$::('zz')` sees it today) — but it would do so for
   *every* value-position block at once, including the `Stmt::SyntheticBlock` wrappers the
   parser emits for `my @a := ...`, whose whole purpose is to declare into the enclosing
   scope. Those would have to be re-routed first.
2. **`scope_isolate` would have to be retired, not translated.** It is a deliberately
   partial revert (§4); `BlockScope`'s policy is a different one. Every `scope_isolate`
   call site needs re-deciding against the fuller policy.
3. **`leave`/`succeed` and the block label have no home in `BlockScope`.** They would need
   either new fields on it or a separate wrapper opcode — i.e. the opcode count does not
   actually drop by one.

The cost of deferring is now bounded in a way it was not before: with one `BlockPlan` and
one skeleton, the merge is a change to `emit_block_shape` and the VM, not a second archaeology
of two drifting passes. The remaining value-position `my` leak is tracked separately.

## 7. Consequences

- `t/block-position-parity.t` pins the three divergences above and the shapes that must
  *not* change (`OUTER::` depth, `state` restart, `leave`/`succeed`, phaser blocks).
- `src/compiler/stmt.rs` loses ~250 lines; the `BlockScope` emitter moves to
  `control_block_scope.rs`, keeping both files inside the 500-line convention.
- A future block-shape question is added in one place. That is the whole point.
