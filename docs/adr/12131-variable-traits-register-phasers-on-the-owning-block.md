# ADR-12131: Variable traits can register phasers on their owning block

- **Status**: Proposed
- **Date**: 2026-10-07
- **Addresses**: [#12131](https://github.com/tokuhirom/mutsu/issues/12131)
- **Related**: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md)
  (compiled declarations and trait dispatch),
  [ADR-0076](0076-bare-block-lowering-and-block-scope-opcodes.md)
  (block-scope phaser queues),
  [ADR-0134](0134-begin-time-prologue.md) (BEGIN-time effects)

## 1. Context

Rakudo applies a variable's `trait_mod:<is>` traits while compiling the
declaration. The handler receives a `Variable` meta-object whose `.block` is
the lexical block that owns the declaration. `Block.add_phaser("ENTER", ...)
` adds work to that block's ENTER queue. The queue runs on every invocation of
the block, including its first invocation.

mutsu currently emits `ApplyVarTrait` at the declaration's run-time position.
Its `Variable` object exposes `var` and `name`, but no owning block, and the
block phaser queues are already being executed by then. Adding a `.block`
method alone would therefore expose the wrong lifecycle: a phaser registered
while executing a declaration cannot run on that same block entry.

ADR-0076 assigns each phaser-bearing block its own scope op and queues. ADR-0134
establishes compiled BEGIN-time effects and explicitly leaves open whether
trait-level effects should join that mechanism. This issue is that open
question for variable traits with an observable block target.

The reference behavior is:

```raku
multi trait_mod:<is>(Variable:D $v, :$foo!) {
    $v.block.add_phaser("ENTER", { say "enter" });
}
{ my Int $x is foo; }
```

The handler runs during compilation, and `enter` is printed when the enclosing
block is entered. If that block is called repeatedly, its registered phaser
runs on each entry.

## 2. Proposed decision

Variable traits that dispatch to `trait_mod:<is>` are compile-time declaration
effects. Their handler must run before the owning block can be entered, and
the `Variable` object passed to it identifies that lexical declaration and its
owning block. A `Block.add_phaser` call updates the owning block's phaser plan;
the compiler emits that plan through the existing block-scope bytecode path,
so the phaser runs on every entry, including the first.

This is a compiler and VM feature, not a run-time AST walk or a per-declaration
fallback. It does not make an already-running block retroactively re-enter or
run a newly appended phaser.

The implementation must preserve declaration scope and source ordering: a
trait on a nested block's variable updates that nested block, and handlers
must observe the same lexical visibility and trait dispatch rules as other
`trait_mod:<is>` applications. Built-in container/type traits retain their
existing behavior where they do not dispatch to user handlers.

## 3. Alternatives

- **Add `.block` to the current run-time `Variable`.** Rejected: the declaration
  runs after its block's ENTER queue, so the phaser cannot run on the current
  entry. It would also expose an execution-time object where Rakudo exposes a
  compile-time target.
- **Append to a live block queue at the declaration site.** Rejected as the
  primary mechanism for the same first-entry ordering problem. It also makes
  the result depend on whether the declaration has executed before a later
  block invocation.
- **Keep `Variable.block` unavailable.** This preserves the current mismatch
  and prevents upstream traits such as Injector's from working.

## 4. Consequences and implementation questions

- The block needs a compiler-visible identity/plan that a variable trait can
  target before that block's first execution. `Block.add_phaser` must append
  to the same ENTER queue consumed by the block-scope lowering in ADR-0076.
- Running user trait handlers at compile time is code execution. It must use
  the established BEGIN-time execution boundary and must remain disabled on
  analysis-only paths, consistent with `docs/security.md` and ADR-0134.
- A follow-up implementation must test the first block entry, repeated
  entries, nested lexical ownership, and ordering with statically declared
  ENTER phasers. It must also run Injector's `t/02-test.rakutest` fixture.
- The concrete representation of the compile-time `Variable` and `Block`
  objects, and how the declaration's compiled trait arguments are made
  available at that phase, remain implementation design questions. Resolve
  them before changing the ADR status to Accepted; do not use a run-time
  mutation workaround.

## 5. Implementation status (2026-10-10)

A first slice is implemented. It is narrower than section 2 and does not
settle the questions above, so the status stays Proposed.

- ADR-0134 already lifts the user variable traits of a nested scope into the
  BEGIN prologue (#12278), where each handler runs once, before the scope is
  entered. That is the compile-time execution path section 4 asks for.
- `Variable.block` answers a `Block` handle. `Block.add_phaser("ENTER", code)`
  files `code` under the declaration's site (`__var_trait_site_N`, set around
  the lifted trait calls by the `__site` synthetic trait).
- The Walker prepends an `ENTER`, `LEAVE`, `KEEP` and `UNDO` phaser to the
  declaring scope (`Frame::entry`). Each replays the phasers of its kind filed
  under the declaration's site, so they run in the block's own queues: `ENTER`
  before the body, the exit kinds on exit (reverse of the order added, as
  Rakudo orders them). `Variable.var` is bound to that frame's variable. The
  declaration stashes the slot before it runs and puts a defined value back
  (`__seeded_decl`), so a value the `ENTER` phaser assigned survives it, as
  with Rakudo's pre-allocated lexical.
- A unit-level declaration is split into its static half and a `BEGIN`
  that applies the traits over it (`lift_unit_var_traits`, before the unit's
  block phasers are split off), and the unit's own queues get the replays. A
  class or package body hoists the declaration, the trait calls and the
  `ENTER` replay to the head of the body and appends `LEAVE`/`KEEP`/`UNDO`
  and the loop kinds after them (`lift_package_var_traits`). The run-time half
  of a split body now wraps its block phasers in a block of its own, so a
  class body's `ENTER`/`LEAVE` run around its statements when the prologue
  declared the class ahead.
- `FIRST`, `NEXT`, `LAST`, `PRE` and `POST` are accepted too. Rakudo does not
  check the verdict of a `PRE`/`POST` added this way, so neither does mutsu.
- A trait that none of these reach (an `EVAL` string, a role body) runs at the
  declaration: an `ENTER` phaser runs at once and other kinds are refused.
- Still open: the compile-time `Block` identity section 4 describes, which
  would let `Block.add_phaser` target an enclosing block rather than the
  declaring one.
