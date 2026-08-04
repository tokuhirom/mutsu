# A block that merely contains a CATCH/CONTROL phaser is not a `try`

```raku
{ die "x"; CONTROL { } }; say "after"
```

rakudo dies here. mutsu printed `after`.

Any Raku block or routine body may carry a `CATCH` or `CONTROL` phaser, and the
phaser needs a protected region to observe, so the compiler wraps such a body in
the very same `OpCode::TryCatch` it emits for a real `try`. The opcode carried no
record of which of the two it was, and the VM's generic exception arm ended with
"if there is an explicit `CATCH` and nothing matched, re-throw; otherwise
swallow". An implicit `CONTROL`-only wrapper has no explicit `CATCH`, so it took
the swallowing branch and ate an exception nothing had handled.

`TryCatch` now carries a `traps` flag, set only by the `try` statement/expression
(`Compiler::compile_try`) and never by the implicit wrapper
(`Compiler::compile_implicit_try`, which is what the ten block/routine/do
call sites use). Only a trapping region swallows; anything else re-throws.

## A `CONTROL` block only *handles* what it matched

With that in place the second half of the rule became implementable. A `CONTROL`
block always runs, but it handles the signal only when a `when`/`default` inside
it matched — exactly the `when_matched()` test the neighbouring `CATCH` arm has
always applied. mutsu treated every `CONTROL` block as handling unconditionally,
so `next; CONTROL { }` silently exited 0 instead of reporting "next without loop
construct".

Declining now routes the signal onward as if the block had no `CONTROL` at all.
For an *illegal* loop-control signal there is nothing further up to consume it,
so it goes to this region's own `CATCH` handling — which is where rakudo's
catchable `X::ControlFlow` comes from, and it only lands in `$!` when the region
really is a `try`:

```raku
next; CONTROL { }                                 # dies, as rakudo does
my $h = ''; try { CONTROL { $h = 'ok' }; next };  # $h eq 'ok', $! ~~ X::ControlFlow
```

That required lifting the `CATCH`-dispatch half of `exec_try_catch_op_inner`
into its own `dispatch_to_catch_handler`, so the declining `CONTROL` path can
reach it instead of duplicating it.

Pin: `t/implicit-catch-wrapper-does-not-trap.t`.

## What it unblocked, and the bug that fix in turn unmasked

The swallow was the blocker recorded in
`todo/deep/implicit-catch-control-wrapper-swallows.md`, and what it blocked was a
one-line parser fix: `body_has_placeholder_vars` (`src/parser/primary/misc/lambda.rs`)
did not gate its `$^`/`@^`/`%^` scan on `depth == 1`, though the sibling
`body_references_topic` right above it does. A placeholder belongs to the
innermost block enclosing it, so `{ status => sub { 0 != $^a } }` is a `Hash` in
rakudo and was a `Block` in mutsu. The gate is in, pinned by
`t/hash-literal-nested-placeholder.t`.

Under `MUTSU_REAL_TEST=1` this closes `roast/S29-context/die.t` and
`roast/S04-exception-handlers/control.t`.

It also made an assertion real that had been passing for the wrong reason.
`t/parse-error-multibyte-column.t`'s third case is written as

```raku
is_run 'my $x = 1 1;', { status => sub { 0 != $^a }, err => rx/'SORRY'/ }, '...';
```

— the exact shape above, so `is_run` had been receiving a `Block` where it wanted
a matcher `Hash`, matching its no-matcher candidate and checking nothing. With
the composer fixed the assertion runs, and mutsu fails it: **mutsu does not
diagnose two terms in a row**, so `my $x = 1 1;` parses, evaluates the first term
and warns about the second in sink context. That gap is recorded in
`todo/tickets/two-terms-in-a-row-is-not-a-parse-error.md`; the pin was re-pointed
at `my $x = "abc"" ;`, the ASCII counterpart of the two multi-byte cases it sits
beside, which is what that file is actually about.
