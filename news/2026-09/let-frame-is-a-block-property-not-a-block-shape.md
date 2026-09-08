# A `let` save frame is something a block *has*, not a kind of block it *is*

`let`/`temp` save their target and resolve at the end of the enclosing block:
the saves are discarded when the block succeeds and rolled back when it fails.
Which block that is, and whether it gets the `OpCode::LetBlock` frame that does
the resolving, was decided by `Compiler::has_let_deep` — and it walked only the
statement kinds that hold an expression *directly* (`Stmt::Expr`, `Stmt::Call`,
`Stmt::Say`/`Print`/`Note`, plus the `Stmt::Block`/`Stmt::If` recursions). A
`let` reached through a declaration's or an assignment's initializer was
invisible to it:

```raku
my $a = 42;
{ my $seen = $( let $a = 23; $a ); Nil };
say $a;      # raku: 42   mutsu: 23
```

The failure mode was not "the save resolves somewhere else". Item context
`$( ... )` is a synthesized `Expr::DoBlock`, which since GH-7635 deliberately
owns no save frame and defers to the real enclosing block — and that block, not
being classified as a `let` block, had no frame either. So *nothing* resolved
the save and the speculative value simply became permanent. The
expression-statement spelling of the very same code (`{ $( let $a = 23; $a );
Nil }`) was correct, because `Stmt::Expr` was one of the arms that existed.

## Two halves, both needed

Adding the missing `Stmt::VarDecl` / `Stmt::Assign` arms (and, one level down,
an `Expr::CompoundAssign` arm, which is how `$t += $( let $a = 23; 1 )` parses)
fixes the divergence — but on its own it hands the newly-classified blocks a
worse deal than the one they had. `BlockShape::LetScope` was an *alternative* to
`BlockShape::Plain`: it emitted `OpCode::LetBlock` around the body statements
**instead of** the position's ordinary scope opcode, so every block the new arms
newly classified would have traded its env restore for a save frame. That is
visible directly:

```raku
my $x = 42;
{ let $x = 1; my $*dyn = 'inner'; Nil };
say $*dyn.defined;   # raku: False   mutsu (before): True
```

So the shape is now the plain shape *plus* a frame. In value position that was
already the arrangement (`LetBlock` wrapping `DoBlockExpr`); statement position
now nests the frame **inside** `OpCode::BlockScope`, around the body statements,
via `Compiler::emit_body_let_frame`. Inside rather than outside is load-bearing:
`exec_let_block_op` decides success from the block's own value, the statement
form delivers that value through the topic (`compile_last_stmt_as_topic`), and
`BlockScope`'s exit deliberately does not write a block-bound topic back to the
enclosing scope — a frame placed outside would read the *enclosing* topic and
decide from the wrong value.

The net effect is that `BlockShape::LetScope` stopped being a shape that
replaces the plain lowering and became one that decorates it, which is what
[ADR-0076](../../docs/adr/0076-bare-block-lowering-and-block-scope-opcodes.md)'s
layering question wanted for this corner.

Pin: `t/let-block-initializer-classifier.t` (14 assertions, every one measured
against `raku` first), alongside the existing `t/do-block-let-resolution.t`
which keeps the GH-7635 boundary — a synthesized wrapper still defers, a genuine
`do { ... }` still resolves at itself.
