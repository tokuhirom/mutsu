# A bare block's shape is decided once, not once per compile pass

A bare `{ ... }` block was the last control construct compiled by two fully
independent passes: the `Stmt::Block` arm of `Compiler::compile_stmt` for
statement position, and `compile_do_block_expr` for value position. The
`for`/`if` unification recorded in
`unify-statement-expression-control-construct-compilation.md` did not sweep the
block pair up, because those two constructs shared a single opcode and a single
skeleton and the block forms share neither — statement position emits
`OpCode::BlockScope`, value position `OpCode::DoBlockExpr`.

GH-7569 asked whether those two opcodes should be merged. The answer, recorded in
[ADR-0076](../../docs/adr/0076-bare-block-keeps-two-opcodes-one-shape.md), is no.
They carry genuinely different runtime contracts — `BlockScope` is a lexical
scope boundary that clones the env, pushes an `$OUTER::` frame and an anonymous
callframe, and runs seven PRE/ENTER/body/KEEP/UNDO/POST ranges; `DoBlockExpr` is
a value-producing body that catches `leave`/`succeed` and deliberately does *not*
restore the env. Merging them would impose the seven-section machinery on the far
hotter of the two, since every `do { ... }`, every routine tail block and every
string-interpolation `{ ... }` goes through `DoBlockExpr`.

What genuinely was duplicated is the *classification*: whether the block is an
implicit `try`, whether it needs a phaser block scope, whether it has
`let`/`temp`, whether it scopes imports. Each pass computed that separately, in a
different order and over a different case set, and each wrote its own answer.
That is now one shared function — `Compiler::classify_block_shape` in
`src/compiler/block_shape.rs`, returning a `BlockShape` both passes `match` on.

Sharing it fixed two divergences from Rakudo, neither of which was reachable from
the position that had the logic right.

**A `state` in a `do` block stopped restarting as soon as the block grew a
phaser.** The value form emitted its per-execution `ResetStateLocals` only on the
paths *below* its CATCH/CONTROL and ENTER/LEAVE early returns, so:

```raku
sub f() { do { state $n = 0; $n++; CATCH { default {} }; $n } }
say f(); say f(); say f();   # raku: 1 1 1   mutsu was: 1 2 3
```

The same block without the `CATCH` restarted correctly, and so did both
statement-position spellings — the statement arm computes the reset before its
dispatch. The value arm now does too, which fixes the CATCH and the ENTER shapes
together.

**A `do` block never rolls back a `let`** -- and that one is *not* fixed here.
`Stmt::Block` has a `has_let_deep` branch emitting `OpCode::LetBlock`; the
value form has no such arm, so `my $x = 1; do { let $x = 2; Nil }` leaves `$x`
at 2 where raku restores it to 1. Adding the arm was the obvious completion, and
it was implemented, run against roast, and backed out.

The reason generalises well past `let`. **`Expr::DoBlock` is not a Raku block.**
The parser and about a dozen compiler desugars use it as a generic "run these
statements, yield a value" node -- item context (`$( let $a = 23; $a )`), the
chained-comparison desugar, `cas`, compound-assignment lowering, method-body
wrappers -- and none of those introduce a scope at which a `let` may resolve.
Emitting a `LetBlock` for every `Expr::DoBlock` resolved the save at the
innermost wrapper instead of the enclosing block, which broke the idiom roast
leans on throughout:

```raku
my $a = 42;
{
  is($(let $a = 23; $a), 23, "let() changed the variable");
  Mu;
}
is $a, 42, "let() should restore the variable, as our block failed";
```

That cost 3 subtests in `roast/S04-blocks-and-statements/let.t` and 1 in
`temp.t`, both whitelisted. So the value path folds `BlockShape::LetBlock` into
its plain arm with the reason stated at the fold, and the test file pins the
`$( ... )` direction so a future attempt fails loudly rather than silently. The
genuine `do { let ... }` gap is filed as GH-7635; closing it needs a marker
distinguishing a real source block from a synthesized wrapper on the AST node
itself.

This is the sharpest evidence for the two-opcode decision. The two positions are
not "the same construct compiled twice": one opcode stands for a *Raku block*,
the other for *a statement sequence that yields a value*, only some of whose
instances are blocks. Sharing the classification is sound because a shape is a
question about the body; sharing the *response* to a shape is not, wherever that
response depends on being a real scope.

`t/block-shape-statement-value-parity.t` pins both halves -- the `state` restart
in every value-position shape, and the `$( ... )` `let` scoping -- and all 12
assertions pass unchanged under `raku`.

The differences that remain between the two arms are now exactly the positional
ones — which opcode carries the scope, whether the result is pushed or discarded,
the placeholder rule (statement position binds them as the block's own parameters
per ADR-0048 D3/D6, value position rejects them as `X::Placeholder::Block`), and
the statement-only setup with no value-position counterpart (`SucceedBarrier`,
`seed_user_listop_shadows`, sigilless-type-name scoping).
