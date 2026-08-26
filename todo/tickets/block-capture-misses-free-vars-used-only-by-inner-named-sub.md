# A block's closure capture misses free variables referenced *only* from an inner named `sub`

Found while implementing `Lock::Async.protect-or-queue-on-recursion`
(`news/2026-08/lock-async-recursion-methods-missing.md`) — the doc example for that method declares
a helper `sub` inside the protected block, and the outer lock variable vanished inside it.

## Repro

```raku
my $l = 42;
my &blk = { sub nested() { say "l=$l" }; nested(); };
blk();
```

- raku v2026.06: `l=42`
- mutsu: `Use of uninitialized value element of type Any in string context.` then `l=`

The bug is specifically about **which free variables the block records as captured**, not about
named subs in general:

```raku
# WORKS -- the block body itself also names $l, so $l lands in the capture set
my $l = 42;
my &blk = { say "direct=$l"; sub nested() { say "nested=$l" }; nested(); };
blk();      # direct=42 / nested=42  (correct)

# WORKS -- a bare (not called-as-a-value) block
my $l = 42;
{ sub nested() { say "l=$l" }; nested(); }      # l=42  (correct)
```

So the free-variable scan that builds a `{...}`/`sub {...}` literal's capture set does not descend
into a nested **named `sub` declaration**'s body. Any variable mentioned only there is absent from
the closure's captured env, and reads back as `Any` when the block is invoked as a Callable value
(`blk()`, `call_sub_value`, `.protect`, `.map`, a `Thread` body, ...).

Under `.protect` the same shape fails even more quietly:

```raku
my $l = Lock::Async.new;
$l.protect({ sub nested() { $l.protect({ say "in" }) }; nested(); });
# mutsu: no output at all (the inner .protect is called on an Any)
# raku:  in
```

## Affected files (starting point)

- Wherever a block/sub literal's free variables are computed for closure capture (grep for
  `free_var`, `captured_bindings`, `block_captured_scalars` in `src/compiler/` and
  `src/runtime/runtime_thread.rs`). The scan needs to treat a nested `Stmt::SubDecl` body as part of
  the enclosing block's free-variable set, the way it already treats a nested anonymous block.

## Why it is a ticket, not a deep item

The fix is a scan-coverage gap in one analysis, not a design question — but it is not a one-liner
either, because "free variables of a nested named sub" has to exclude that sub's own parameters and
`my` declarations, and the same set feeds `clone_for_thread`'s shared-variable seeding, so it wants
its own regression test alongside `t/closure-capture-*.t`.
