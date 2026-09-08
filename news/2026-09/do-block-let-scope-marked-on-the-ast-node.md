# A block's `let`/`temp` scope is decided by the AST node, not by a sentinel label

`let` and `temp` save a variable's previous value and resolve that save at the end of the
enclosing **block** — `temp` always restores, `let` restores when the block fails and commits
when it succeeds.

The bare-block unification (GH-7569, ADR-0076) gave the value position the `let`/`temp` branch
the statement position always had, which closed the divergence GH-7635 was filed for:

```raku
my $x = 1;
do { let $x = 2; Nil };
say $x;      # raku: 1
```

It had to answer a second question to do that. **`Expr::DoBlock` is not a Raku block.** Around
forty parser and compiler desugars build one as a generic "run these statements, yield a value"
vehicle: item context `$( ... )`, the chained-comparison temp-var lowering, `cas`, compound
assignment, `.=` writeback, hyper-op destructuring. None of those open a scope at which a save
may resolve, and treating one as a block resolves the save at the innermost wrapper rather than
at the real block — which breaks the idiom roast leans on throughout:

```raku
my $a = 42;
{
  is($(let $a = 23; $a), 23, "let() changed the variable");
  Mu;
}
is $a, 42, "let() should restore the variable, as our block failed";
```

The answer at the time was a **denylist**: a sentinel label
(`STMT_LIST_CONTEXTUALIZER_LABEL`) on the one node known to need the exemption, `$( ... )`, with
every other `Expr::DoBlock` owning a save frame by default.

## The marker moves onto the node, and becomes an allowlist

`Expr::DoBlock` now carries a `DoBlockOrigin`, which is what GH-7635 asked for.
`DoBlockOrigin::SourceBlock` is set at the handful of sites that mint a node from real source
braces that *are* a Raku block:

- the `do` keyword's own braces (`parser/primary/ident/identifier_call.rs`) — the single site
  that parses that form;
- a labelled `L: do { ... }` and a labelled bare `L: { ... }`
  (`parser/stmt/control/labeled_loop.rs`);
- the statement prefixes whose block runs inline in the current frame rather than as a closure —
  `lazy`, `sink`, `quietly` — which re-host the user's own braces in a `DoBlock`;
- the RakuAST round-trip of `StatementPrefixDo` (`rakuast/lower.rs`).

Everything else is `DoBlockOrigin::Desugar`, which is what the new `Expr::desugar_block`
constructor produces, so a future desugar gets the right answer without having to know the rule
exists. That is the point of the direction change: a denylist is only correct for the exemptions
somebody thought to write down, and it was already wrong for one that nobody had.

`BlockPosition::Value` carries the origin, so `BlockPlan::analyze` reads the answer off the
position it is already given and the sentinel label is gone — leaving `label` to mean the
block's label again.

## Two divergences the allowlist fixes

**A string-interpolation block owned a save frame it should not.** `temp` restores
unconditionally at the block that owns the save, so it is the sharp test for whether a construct
is one, and Rakudo says an interpolation block is not:

```raku
my $x = 1;
my $s = "{ temp $x = 2; 'v' }";
say $x;      # raku: 2   mutsu (before): 1
```

Interpolation builds `DoStmt(Stmt::Block(...))` and reached the value-position block lowering
with no exemption. It is `Desugar` now, with the measurement recorded at the site.

**A value-position `let` block clobbered the enclosing topic.** `OpCode::LetBlock` reads the
block's own value to decide restore-vs-commit, and the two positions leave that value in
different places: the statement form routes its last statement through
`compile_last_stmt_as_topic`, so the op read `$_`; the value form had the value on the stack and
was made to fit by emitting `Dup; SetTopic`. That wrote the enclosing scope's topic:

```raku
my $x = 1;
$_ = 'topic';
do { let $x = 2; 99 };
say $_;      # raku: topic   mutsu (before): 99
```

The opcode now carries `value_on_stack`, and `exec_let_block_op` peeks the stack top instead of
reading the topic when it is set. No `Dup`/`SetTopic`, no collateral damage.

## Two consequences of widening the node

The AST is what the precompilation cache stores, so an older build's cached `DoBlock`
deserializes a field short — `precomp.rs`'s `CACHE_FORMAT_VERSION` goes to 11 accordingly.

`rakuast/convert.rs` now refuses to convert a `Desugar` node to `StatementPrefixDo`. It would
otherwise round-trip back through `lower.rs` as a genuine source block, handing back a node with
block semantics the original never had.

Pin: `t/do-block-let-resolution.t`, 21 assertions, all of which pass unchanged under `raku`.

## Still open

Two neighbouring divergences share the subsystem but not the root cause, and are filed
separately rather than widened into this change:

- GH-7645: `Compiler::has_let_deep` does not look inside a declaration's or an assignment's
  initializer, so `{ my $seen = $( let $a = 23; $a ); Nil }` never classifies its enclosing block
  as a `let` block at all.
- GH-7646: a routine body does not resolve its own saves — `sub f() { let $x = 2; Nil }` leaves
  `$x` at 2 where `raku` restores it to 1.
