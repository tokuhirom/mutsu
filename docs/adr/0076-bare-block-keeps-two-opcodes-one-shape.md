# ADR-0076: A bare block keeps two opcodes, but has one shape classifier

- **Status**: Accepted
- **Date**: 2026-09-08
- **Related**: [GH-7569](https://github.com/tokuhirom/mutsu/issues/7569),
  `src/compiler/block_shape.rs`, the `Stmt::Block` arm of `src/compiler/stmt.rs`,
  `src/compiler/helpers_do_expr.rs`, `OpCode::BlockScope` / `OpCode::DoBlockExpr` /
  `OpCode::LetBlock` in `src/opcode.rs`, `src/vm/vm_misc_scope.rs`,
  `src/vm/vm_misc_block.rs`,
  `news/2026-09/unify-statement-expression-control-construct-compilation.md`

## 1. Context

The campaign recorded in
`news/2026-09/unify-statement-expression-control-construct-compilation.md` gave every
statement/expression control construct a single lowering shared by both source positions:
`for` (`src/compiler/control_for.rs`) and `if`/`elsif` (`src/compiler/control_if.rs`) each
have one, and `while` / C-style `loop` / `lazy for` in expression position are thin
`gather`-desugaring wrappers over the statement path.

The bare `{ ... }` block was the one construct left compiled by two independent passes:

- **statement position** — the `Stmt::Block` arm of `Compiler::compile_stmt`, emitting
  `OpCode::BlockScope`;
- **value position** — `compile_do_block_expr` / `compile_do_block_expr_scoped`, emitting
  `OpCode::DoBlockExpr`.

GH-7569 asked the question that has to be settled before any unification lands: **should
`BlockScope` and `DoBlockExpr` be one opcode?** The two arms share neither an opcode nor a
skeleton, which is exactly why the `for`/`if` unification did not sweep them up.

## 2. Decision

**Two opcodes, one shape classifier.**

1. `BlockScope` and `DoBlockExpr` stay separate opcodes with separate runtime contracts
   (§3). This is not a deferral: it is the answer.
2. The *classification* both passes were computing independently — "what kind of block is
   this" — becomes a single shared function,
   [`Compiler::classify_block_shape`](../../src/compiler/block_shape.rs), returning a
   `BlockShape` (`ImplicitTry` / `PhaserScope` / `LetBlock` / `ImportScope` / `Plain`).
   Both passes `match` on it, in the same order, over the same case set.
3. The per-execution `state` reset is emitted **before** the shape dispatch in both passes,
   as an unconditional part of the skeleton rather than a per-arm decision.

## 3. Why the opcodes stay separate

The pair looked like duplication, but the two ops carry genuinely different runtime
obligations, and the cheap one is by far the hot one.

`BlockScope` is a **lexical scope boundary**. `exec_block_scope_op` clones the env, scans
its own opcode range for `SetLocalDecl` / `DeclareOurScalar` / `StateVarInit` slots and for
topic-binding ops, pushes an `outer_scope_locals` frame, a block-scope depth, a lexical
class scope, an enum scope and a once scope, snapshots the routine registry, pushes an
anonymous callframe when the block is a genuine source `{ ... }`, and runs seven ranges —
PRE, ENTER, body, KEEP, UNDO, POST — which is what its seven patch points are for. It
produces no value.

`DoBlockExpr` is a **value-producing body with control-flow catching**. It pushes a once
scope and an enum scope, optionally snapshots the routine registry (only when the body
actually declares one), catches a matching `leave` and a `succeed` and turns each into the
block's value, and deliberately does *not* restore the env in the common case.

Merging them would mean giving every value-position block the seven-section, env-cloning,
callframe-pushing machinery. That is the wrong direction on cost: `DoBlockExpr` is emitted
for every `do { ... }`, every routine tail block and every string-interpolation `{ ... }`,
so it is executed far more often than `BlockScope`, and the union variant would also have
to carry both payloads while `size_of::<OpCode>() <= 48` stays pinned by
`opcode_size_guard`.

The reverse direction — expressing the statement form as the value form with the result
discarded — fails on semantics rather than cost. The statement form's env restore is what
stops a block-local `my` from leaking; the value form deliberately lets an outer mutation
inside `do { ... }` persist (its `scope_isolate` variant exists precisely because *some*
callers want the restore and most must not have it). Collapsing the statement form onto the
value form would either drop the restore or impose it on every `do`.

So the shared thing is not the opcode. It is the *decision* — and that is what the two
passes were getting wrong.

## 4. What the split classification actually cost

Two divergences, both real bugs against Rakudo, both fixed by the shared classifier rather
than patched individually:

**The value form's `state` reset sat after its early returns.** `compile_do_block_expr`
returned early for the CATCH/CONTROL and ENTER/LEAVE shapes, and only computed
`emit_value_block_state_reset` on the paths below them. So a `state` counter restarted per
execution in `do { state $n = 0; $n++; $n }` but persisted in the same block with a `CATCH`
or an `ENTER` added:

```
sub f() { do { state $n = 0; $n++; CATCH { default {} }; $n } }
say f(); say f(); say f();   # raku: 1 1 1   mutsu (before): 1 2 3
```

The statement form always got this right, because it computes the reset before its
dispatch. Hoisting the value form's reset to the same place fixes both phaser shapes at
once.

Pinned by `t/block-shape-statement-value-parity.t`, whose assertions all pass under `raku`
unchanged.

The second divergence the split had hidden — the value form having no `let`/`temp` arm at
all, so `my $x = 1; do { let $x = 2; Nil }` leaves `$x` at 2 where raku restores it to 1 —
is **real but not fixed here**, for the reason in §5.

## 5. `BlockShape::LetBlock` is a shape the value path declines

The obvious completion of §4 — give `compile_do_block_expr` the `LetBlock` arm the statement
form has — was implemented, measured against roast, and backed out. It is recorded here
because the reason generalises well beyond `let`.

**`Expr::DoBlock` is not a Raku block.** The parser and about a dozen compiler desugars use
it as a generic "run these statements, yield a value" node: item context
(`$( let $a = 23; $a )`), the chained-comparison desugar (`src/chain_compare.rs`), `cas`,
compound-assignment lowering, method-body wrappers. None of those introduce a scope at which
a `let` may resolve. Emitting a `LetBlock` for every `Expr::DoBlock` resolved the save at the
innermost wrapper instead of the enclosing block, and broke the idiom roast leans on
throughout:

```raku
my $a = 42;
{
  is($(let $a = 23; $a), 23, "let() changed the variable");
  Mu;
}
is $a, 42, "let() should restore the variable, as our block failed";
```

That cost 3 subtests in `roast/S04-blocks-and-statements/let.t` and 1 in `temp.t`, both
whitelisted. So the value path keeps `BlockShape::LetBlock` folded into its plain arm, with
the reason stated at the fold, and `t/block-shape-statement-value-parity.t` pins the
`$( ... )` direction so a future attempt fails loudly instead of silently.

This is the sharpest evidence for §3's conclusion. The two source positions are not "the
same construct compiled twice": one of the two opcodes stands for a **Raku block**, and the
other stands for **a statement sequence that yields a value**, only *some* of whose
instances are blocks. Sharing the classification is sound precisely because a shape is a
question about the body; sharing the *response* to a shape is not, wherever the response
depends on being a real scope.

Closing the gap needs a marker distinguishing a genuine source block from a synthesized
wrapper, carried on the AST node — tracked as
[GH-7635](https://github.com/tokuhirom/mutsu/issues/7635). `ast.rs`'s own
placeholder-classification comments flag the same overloading from a different angle, so the
marker would likely pay for itself more than once.

## 6. Consequences

- The two passes can no longer disagree about what a block *is*. A body that is an implicit
  `try` in one position is an implicit `try` in the other, and a shape added to
  `BlockShape` is offered to both arms rather than to whichever one the author was editing.
  Where an arm declines a shape (§5), that is now a stated decision at a single site rather
  than an absence nobody had noticed.
- The remaining differences between the arms are now exactly the ones that are genuinely
  positional, and they are visible as such: which opcode carries the scope, whether the
  result is pushed or discarded, the placeholder rule (statement position binds them as the
  block's own parameters per ADR-0048 D3/D6; value position rejects them as
  `X::Placeholder::Block`), and the statement-only setup that has no value-position
  counterpart (`SucceedBarrier`, `seed_user_listop_shadows`, sigilless-type-name scoping).
- The dispatch *order* is now a documented property of one enum rather than an emergent
  property of two `if`/`else if` chains. The order is load-bearing — a body with both a
  `CATCH` and a `use` is an implicit `try`, and the import scope is not applied — and both
  passes have always agreed on it, so it is preserved verbatim rather than "fixed".
- GH-7569's premise that a full opcode merge might be required is answered: it is not, and
  a future session should not re-open it without new evidence of a cost the split imposes.
