# A block with only a `CATCH`/`CONTROL` swallows what should escape it

```raku
{ die "x"; CONTROL { } }; say "after"
```

rakudo dies. mutsu prints `after`. The block is not a `try`, so nothing there
should trap the exception — but mutsu compiles every block containing a `CATCH`
or `CONTROL` through `Compiler::compile_try`, and the resulting `OpCode::TryCatch`
carries no flag saying whether it is a genuine `try` or an implicit wrapper. The
handler in `vm/vm_try_catch_ops.rs` therefore ends its generic error arm with
"if `explicit_catch` and nothing matched, re-throw; otherwise swallow" — and an
implicit `CONTROL`-only wrapper takes the swallowing branch.

## Why it matters beyond the `die` case

It blocks the correct rule for a `CONTROL` block that matches nothing. rakudo:

```raku
next; CONTROL { }                       # dies: "next without loop construct"
my $h = ''; try { CONTROL { $h = 'ok' }; next }   # $h eq 'ok', $! ~~ X::ControlFlow
```

The handler body *runs* in both cases, but a `CONTROL` block only **handles**
the signal when a `when`/`default` matches — exactly the rule the `CATCH` arm
next to it already implements via `when_matched()`. Adding that test to the
`CONTROL` arm is a five-line change and makes the first line above correct, but
the second one then dies instead of being trapped: the `CONTROL` block and the
`try` are the *same* `TryCatch` op, so declining has to fall through to that
op's own catch handling rather than return `Err`. Routing it there today means
routing it into the swallow above — which would silently eat
`roast/S04-exception-handlers/control.t`'s first assertion again.

So the order is: give `TryCatch` a "this region really traps" flag (set only by
the `try` statement/expression, not by the implicit `has_catch_or_control`
wrapper), fix the generic arm to swallow only for that, and only then add the
`when_matched` test to the `CONTROL` arm.

## What is blocked on it

`t/hash-literal-nested-placeholder.t` and the parser change it pins: a
placeholder inside a *nested* block belongs to that block, so
`{ status => sub { 0 != $^a } }` is a `Hash` in rakudo and a `Block` in mutsu
(`body_has_placeholder_vars` in `src/parser/primary/misc/lambda.rs` does not gate
its `$^`/`@^`/`%^` scan on `depth == 1`, though the sibling
`body_references_topic` right above it does). That one-line gate is correct and
closes `roast/S29-context/die.t` under `MUTSU_REAL_TEST=1` — but it makes two
whitelisted files start really checking assertions that had been passing for the
wrong reason, and one of them is `control.t`'s `next; CONTROL { }`.

The patch and its pin are small enough to re-derive: gate the placeholder scan
on `depth == 1`, and assert that `{ status => sub { 0 != $^a } }`,
`{ status => { $^a } }` and `{ a => 1, b => sub { $^x } }` are all `Hash` while
`{ a => $^x }` and `{ $^x }` stay `Callable`.
