# A `&`-sigil lexical now shadows a builtin of the same name

```raku
my &emit = { "e" };
sub f() { my @out = 1; @out.push(emit()); @out }
say f().raku;
```

used to reach mutsu's **builtin** `emit` (the `gather`/`supply` control-flow
one) instead of the lexical `&emit` plainly in scope. Because that builtin is
a control-flow construct, the call did not merely return the wrong value — it
hijacked the enclosing routine, returning `Nil` instead of `[1, "e"]`.

`emit` and `done` are exactly the two names roast's `Test::Tap::tap-ok` takes
as `:&emit` / `:&done` and invokes as `emit() if &emit; ... done() if
&done` from inside its tap callback. The general rule is the one already
established for qualified calls in
`news/2026-07/qualified-call-no-longer-aliases-a-builtin.md`: resolve on
**whether a declaration exists**, not on whether the name happens to be a
builtin. A lexical `&name` in scope is a declaration and must win.

## Three separate gaps, one root cause

Investigation turned up three independent code paths that each hardcoded
`emit`/`done`/`push`-style names to special syntax without checking whether a
declaration shadowed them:

- **The `done()` statement parser.** `known_call_stmt` unconditionally
  rewrote a bare `done()` (and the bareword `done` term) to the react/supply
  completion signal `Stmt::ReactDone` / `OpCode::ReactDone`, regardless of
  scope. `emit()` never had this problem — it stays an ordinary call at parse
  time — which is why the ticket's own `my &emit` repro already worked before
  this change while every `done` equivalent still failed. Fixed by gating
  both special forms on `!is_user_declared_sub("done")` /
  `!self.amp_binding_in_active_scope("done")`.

- **Signature `&name` params were invisible to the parser's scope tracker.**
  `&emit`/`:&done` in a signature were never registered as user subs, so
  nothing downstream (including the `done` fix above) could see them —
  including from a *nested closure* that captures the enclosing routine's
  parameter, exactly the shape `Test::Tap::tap-ok` uses (`:&emit, :&done`
  params; the tap callback is a separate block that calls them). Fixed by
  registering `&name` signature params into the same parse-time scope the
  sigilless-param mechanism already used, pushed for the routine body
  (`sub_decl.rs`).

- **A non-final statement call skipped the local-binding check entirely.**
  This was the deepest one, and not specific to `emit`/`done`: expression-
  position calls (`compile_expr_call_inner`) already checked
  `amp_binding_in_active_scope` and rerouted to the closure's `CodeVar`
  before ever reaching name-based runtime dispatch — but `Stmt::Call`
  (a *non-final*, sunk statement in a block) had no equivalent check, so it
  fell straight to `ExecCall`'s purely name-based resolution. That resolution
  checks user-*registered subs* first, but a lexical closure binding is not a
  registered sub, so it reached the actual builtin. Concretely:
  `{ emit() if &emit; @res.push($_) }` — literally `Test::Tap`'s tap
  callback shape — called the *real* `emit` builtin and crashed with `emit
  without supply or react`, even though the exact same call as the block's
  *last* statement resolved correctly. Fixed by adding the same
  `amp_binding_in_active_scope` check to `Stmt::Call`'s compile path,
  delegating to the already-correct expression-call compiler when a lexical
  binding shadows the name.

## What's still open

A bare `emit(...)` written *directly inside* a `supply { ... }` block's own
body (not a closure it calls out to) still ignores an in-scope lexical
`&emit` — that rewrite is a parse-time syntactic transform with no runtime
equivalent to fall back to, so it needs a different fix shape. Tracked in
`todo/tickets/emit-inside-supply-block-ignores-a-lexical.md`.

## Verification

`t/lexical-shadows-builtin-call.t` (15 subtests) pins all three fixes,
including the nested-closure `Test::Tap` shape and the non-final-statement
case. It passes unchanged under rakudo, so it is a differential test rather
than a record of mutsu's own output.
