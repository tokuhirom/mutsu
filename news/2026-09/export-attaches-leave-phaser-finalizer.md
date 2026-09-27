# A module can attach a LEAVE phaser to the scope that uses it (FINALIZER loads and passes)

FINALIZER 0.0.10 used to be `blocked_load`: `'LeavePhaser' cannot inherit from
'RakuAST::StatementPrefix::Phaser::Leave' because it is unknown`. Its three test files now pass,
matching rakudo.

The module finalizes resources when the scope that `use`d it is left. Its `sub EXPORT` asks the
compile-time resolver for that scope and hands it a phaser:

```raku
($*R.find-attach-target('block') // $*R.find-attach-target('compunit'))
  .add-leave-phaser: LeavePhaser.new(&FINALIZE)
```

mutsu's compile-time surface is the RakuAST-shaped one, not a `$*W` World (ADR-0098 §2.1), so this
is the branch it now honours (`src/runtime/attach_target.rs`). mutsu runs EXPORT when the `use`
statement executes, and that happens inside the importing block's import scope, once per entry. So
"attach a LEAVE to the block" becomes "queue this callable on the block's import scope", and the
scope runs the queue, last attached first, however the block is left. The attach targets follow
rakudo's RakuAST frontend: `'block'` is the innermost block or routine body and is `Nil` at a
compunit's top level, and `'compunit'` is the module body, `EVAL` string or main program that holds
the `use`.

For that to hold on every exit, every block that directly contains a `use` now opens an
`ImportScope` region. That covers bare blocks, blocks inlined in tail position, and loop bodies.
Before this, bare blocks used a push/pop opcode pair, and the pop never ran on `die`, `next` or
`return`, so the block's imports leaked too. The pair is gone.

Getting FINALIZER's suite to run exposed four unrelated bugs, each fixed generally:

- **RakuAST node classes are subclassable.** `RakuAST::Block.new` defaults its body, and each
  `StatementPrefix::Phaser::<Kind>.new($block)` constructs its node. A subclass instance does not
  yet carry the node its parent constructor builds (#9761).
- **`multi sub EXPORT` dispatches on every import.** A later `use` with different arguments
  re-ran whichever candidate the first import picked.
- **A module's `my class` survives the block that first loaded it.** Closing that block's import
  scope deleted the class, because its mangled lexical name looked like an import alias. The
  module's EXPORT then failed on the next `use`.
- **`self` in an attribute initializer is the constructed object.** It used to be a snapshot
  instance, so a closure created there (`has &!f = register({ self.finalize })`) saw neither the
  object's identity nor the attributes set after it. This now holds for `new`, `bless` and the
  native default constructor.
- **`Lock.protect` with a block passed in.** The inline fast path seeded the block's captured
  variables from the frame calling `.protect`. A block created elsewhere therefore read the
  callee's same-named locals: `method !protect(&code) { $!lock.protect: &code }` gave the block its
  own `&code`. The fast path now applies only to block literals of the calling frame.
