# A `when`/`default` succeed with no topicalizer no longer crashes

`do when COND { ... }` used in expression position — e.g. `$a = do when .so
{ "foo" }` with no enclosing `given`/`with` — crashed mutsu with a bare
`Runtime error:` and no message. Two shapes hit it: a bare block wrapping the
assignment (`{ $a = do when .so { "foo" } }`), and the true mainline with
nothing enclosing at all (`$_ = True; my $a = do when .so { "foo" }; say
$a;`). Real `raku` does neither: the first prints `(Any)` and the second
prints nothing at all — the program just ends right there, silently.

## Root cause

A *matching* `when`/`default` clause does not hand its body's value to
whatever's waiting for it (a pending assignment, in this case); it runs the
block and then raises the `succeed` control signal (`RuntimeError::control =
Control::Succeed`, `src/value/error.rs`), which unwinds to the nearest
enclosing **topicalizer** (`given`/`with`) if there is one. mutsu already
handled that case correctly (`exec_given_op` / `exec_do_given_expr_op` in
`src/vm/vm_given_when_ops.rs`).

What was missing is what Raku does when there is **no** topicalizer: the
succeed is absorbed by the nearest enclosing **block-like construct**
instead — a bare block, an `if`/`unless` branch, a loop body, a `try`, a sub
body, or, with nothing else left at all, the compilation unit itself. mutsu
already had unconditional absorbers for several of these (a sub call's own
catch in `vm_closure_dispatch.rs`, a loop's per-iteration catch, `given`,
`do {}`'s `exec_do_block_expr_op`) — verified against real `raku`, they were
already correct. Two boundaries were missing an absorber entirely, and a
third had an absorber whose static detection was incomplete:

1. **A bare `{ ... }` block statement.** The compiler already had a
   mechanism for this — `OpCode::SucceedBarrier`, wrapping the block's
   bytecode — but it was gated on `body_has_toplevel_when`, a scan that only
   recognized a *literal* top-level `Stmt::When`/`Stmt::Default`. `do when`
   is an ordinary term and can appear at any expression-nesting depth (an
   assignment RHS, a call argument, a list element, string interpolation,
   ...), so `{ $a = do when .so { "foo" } }` — where the `when` is buried
   inside an `Assign` statement's RHS, not a literal `Stmt::When` — was
   invisible to the scan, and the block never got its `SucceedBarrier`. The
   same gap applied to `if`/`unless` branches and loop bodies, which share
   the same detection helper (`with_succeed_barrier` in
   `src/compiler/helpers_control_flow.rs`).

2. **The true mainline.** `Interpreter::run()` (`src/runtime/run.rs`) had no
   catch at all around `run_top()`'s result — an escaping succeed just
   became the crash.

3. **`try`.** Turned out to never have absorbed a `when`'s succeed at all,
   for any shape, literal or nested: `exec_try_catch_op_inner`
   (`src/vm/vm_try_catch_ops.rs`) explicitly listed `is_succeed()` among the
   control signals that "must propagate up — `try` alone does not catch
   them," with a comment that reads as a deliberate design decision. Real
   `raku` disagrees: `try { $_ = True; when .so { "foo" } }; say "after"`
   prints `after` — `try` absorbs the succeed exactly like a bare block,
   *and* takes precedence over an even-further-out `given` (`given 5 { try {
   when 5 { ... } }; say "after-try" }` still runs `after-try`, because
   `try` is the nearer boundary). This is a separate, independently
   verified divergence from the `SucceedBarrier` detection gap — a pure
   VM-side `Result` arm, no bytecode involved — fixed alongside it since the
   deliverable (`t/succeed-block-boundary-absorption.t`) explicitly checks a
   `try` case end to end against real `raku`.

## The fix

- `src/compiler/stmt.rs` (`Stmt::Block`) and
  `src/compiler/helpers_control_flow.rs` (`with_succeed_barrier`, shared by
  `if`/`unless` branches and loop bodies): kept the existing conditional
  `SucceedBarrier` wrap, but replaced the literal-`Stmt::When`-only scan
  with a proper recursive one (`body_has_toplevel_when` /
  `stmt_reaches_when` / `expr_reaches_when`) that walks a statement's own
  expression tree — assignments, call arguments, list/hash literals, method
  calls, binary/ternary/index expressions, string interpolation, ... —
  looking for a `do when`/`do default` term, without crossing into a nested
  scope that already has its own absorber (a nested block/if/loop/sub,
  `given`, `do {}`, `try`, a closure).

  An **unconditional** wrap (drop the scan, always emit `SucceedBarrier`)
  was tried first and is simpler, but it regressed `cargo test`'s
  `jit_diff::unsupported_opcode_bails_out_cleanly`: `SucceedBarrier` is
  outside the JIT's supported opcode set, and since `with_succeed_barrier`
  backs essentially every loop body and `if`/`unless` branch in the
  language, wrapping unconditionally turned ordinary, `when`-free loops and
  branches into permanent JIT bailouts. The conditional scan keeps the
  common case (no `when` anywhere near the block) exactly as fast as
  before, while still being a *complete* check — not just "good enough for
  the ticket's five test cases" — since it recurses through the actual
  expression shapes `do when` can hide behind, not just the one shape a
  hand test happened to use.

- `src/runtime/run.rs` (`Interpreter::run()`): after `run_top()`, a
  `Err(e) if e.is_succeed()` is converted to `Ok(e.return_value)` — the
  compilation unit is the terminal absorber, matching real `raku`'s "the
  program just ends, silently, right there."

- `src/vm/vm_try_catch_ops.rs` (`exec_try_catch_op_inner`): added a
  dedicated `Err(e) if e.is_succeed()` arm, ahead of (and removed from) the
  "propagate control signals through try" arms, that absorbs the succeed —
  truncating the stack, restoring `when_matched()` to its pre-`try` value
  (so an enclosing `given`'s own "break the body after every op on a match"
  bookkeeping isn't fooled into thinking a `when` matched at *its* level),
  and falling off the end of the `try` like a normal completion. Not routed
  through `try`'s own CATCH/CONTROL — `succeed` is not an exception.

## Why this set of boundaries, and not a broader one

Each of the boundaries above was individually checked against real `raku`
before being touched, specifically to avoid silently swallowing a signal
that should have gone somewhere else:

- **A sub body** (`sub f() { my $a = do when .so { "foo" }; ...; return
  "ret" }`) already correctly absorbs the succeed as its own **return
  value** (`"foo"`, not `"ret"` — the statements after the succeed never
  run) via the pre-existing sub-call catch in `vm_closure_dispatch.rs`. Not
  touched; pinned as a non-regression case.
- **A `for` loop body** with no topicalizer ends the **entire loop**, like
  `last` — not just the current iteration — via the loop's own pre-existing
  per-iteration catch. Not touched; pinned as a non-regression case.
- **`given` directly wrapping a matching `when`** still ends the `given`'s
  own body early, exactly as before (`given 5 { when 5 { ... }; say "after"
  }` skips `after`). This is the control case proving the `try` fix doesn't
  overreach: `try` intercepts a succeed nested inside it *before* it would
  ever reach an enclosing `given`, but a `given` with nothing nearer still
  behaves exactly as it always has.

`t/succeed-block-boundary-absorption.t` (15 assertions) pins all of the
above — the two crashing cases from the original report, the already-correct
`given` cases, the sub/for/try non-regression cases, the `try`-nested-in-
`given` precedence case, a regression pin for the pre-existing
literal-top-level-`when`-in-a-nested-block case, and a deep-expression-
nesting case (`do when` inside a list literal) — verified to pass under both
`raku` and mutsu.
