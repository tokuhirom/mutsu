# One bare-block lowering, shared by both source positions

`for` and `if`/`elsif` each got a single lowering shared by their statement and
expression forms (`src/compiler/control_for.rs`, `src/compiler/control_if.rs`). The bare
`{ ... }` block was the one construct left with two independent compilation passes, and
[#7569](https://github.com/tokuhirom/mutsu/issues/7569) tracked it as the residue of that
campaign:

- **statement position** — the `Stmt::Block` arm of `Compiler::compile_stmt`;
- **value position** — `compile_do_block_expr` / `compile_do_block_expr_scoped`
  (`do { ... }`, a block used as a term, the parser's labelled-block lowering).

Both had to decide the same things about the body — does it declare routines? does it
import? does it need a per-execution `ResetStateLocals`? does a `CATCH`/`CONTROL` make it
an implicit `try`? do `ENTER`/`LEAVE`/`KEEP`/`UNDO` make it a phaser block scope? — and
each wrote its own answer. `BlockPlan::analyze` and `Compiler::compile_block_construct`
(`src/compiler/control_block.rs`) are now the one place those questions are answered and
the one skeleton that consumes the answer; `BlockPosition` names everything that genuinely
differs. Both former entry points are thin wrappers. `src/compiler/stmt.rs` loses ~250
lines, and the long `OpCode::BlockScope` emitter moves to `control_block_scope.rs`.

## The drift it was hiding

Measured against `raku` as the oracle, three answers had gone out of sync — and they had
drifted in *both* directions:

| | `raku` | mutsu before |
| --- | --- | --- |
| `our $g = 1; do { temp $g = 2; 1 }; say $g` | `1` | `2` |
| `do { my \str = "hi"; 1 }; say str.^name` | `str` | `Str` |
| `{ use MONKEY-SEE-NO-EVAL; my $y = 42 }; say $::('y')` | `(Any)` | `42` |

The value pass had never grown the `let`/`temp` branch, so a `temp` inside a `do { }` was
never restored, and never grew the sigilless-shadow bookkeeping, so a `my \str` declared
inside one kept shadowing the native type name after the block ended. The statement pass
had drifted the other way: it treated a `use` in the body as an exclusive *shape*, so a
block that imported got an import scope and no block scope at all and leaked its `my`
declarations. All three fall out of the shared plan.

Unifying the import handling also exposed a pre-existing bug it had been hiding. `$!` is
implicitly declared in every Raku scope, and a `try`/`CATCH` in a nested block assigns the
one the enclosing scope sees (`{ try die "b" }; say $!` prints `b` in rakudo). mutsu only
creates the `!` env key once something writes it, so `exec_block_scope_op`'s restore
dropped it as a block-local declaration unless an *earlier* statement had already created
it — `$!` propagated out of a block only by accident. `roast/integration/error-reporting.t`
("Backtrace does not change on additional .backtrace") had been passing on exactly that
accident, through the `use`-shaped block a few tests above it. `$!` now propagates
unconditionally.

Making `$!` propagate in turn exposed a second one, in the same causal chain: a `try` whose
control signal a `CONTROL` `when`/`default` MATCHED did not count as a normal completion,
so it left `$!` at whatever the region had inherited instead of resetting it the way the
plain success path does. `try die "stale"; try { CONTROL { default { } }; next }` then
reported the *stale* error; raku says `Any`. `t/implicit-catch-wrapper-does-not-trap.t` had
been passing on the mirror-image accident — `$!` never propagating out of the earlier block.

## `$( stmt; ... )` is a contextualizer, not a block

Giving the value position a `let`/`temp` scope needed `do { ... }` told apart from
`$( ... )`, which the parser also lowers to an `Expr::DoBlock` because it carries a
statement list. It is not a Raku block: roast's `S04-blocks-and-statements/let.t` and
`temp.t` pin that `{ $(let $a = 23; $a); Mu }` restores `$a` at the *enclosing* block's
exit, not at the closing paren. The lowering now marks itself with the sentinel label
`crate::ast::STMT_LIST_CONTEXTUALIZER_LABEL` (following the existing
`__mutsu_check_phaser__` precedent), which the compiler consumes and never forwards to
`OpCode::DoBlockExpr`.

## The opcodes stay two

[ADR-0076](../../docs/adr/0076-bare-block-lowering-and-block-scope-opcodes.md) records the
decision the issue asked for. `OpCode::BlockScope` and `OpCode::DoBlockExpr` are *not*
merged: §4 writes down what each guarantees about the value stack, the env, the import
scope and the phaser queues, and §6 says why expressing one as the other is a separate
change (it would make every value-position block restore `env`, including the
`Stmt::SyntheticBlock` wrappers whose whole job is to declare into the enclosing scope, and
`scope_isolate` would have to be re-decided per call site rather than translated). With one
`BlockPlan` and one skeleton, that merge is now a change to `emit_block_shape` and the VM
rather than a second archaeology of two drifting passes.

Pin: `t/block-position-parity.t` (30 assertions, all verified against `raku`).
