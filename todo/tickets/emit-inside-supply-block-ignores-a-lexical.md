# A lexical `&emit` is still ignored *inside* a `supply { ... }` block

`todo/tickets/code-lexical-does-not-shadow-a-builtin.md` fixed a lexical
`&emit`/`&done` shadowing the control-flow builtin for an ordinary bare call —
including from a nested closure, and including a non-final statement call.
That fix does not reach one remaining shape: a bare `emit(...)` written
*directly inside a `supply { ... }` block's own body* (not a closure it calls
out to) still ignores an in-scope lexical `&emit` and is rewritten to the
block's own emitter, unconditionally:

```raku
my &emit = { say "lexical emit called with $_[0]"; "e" };
my $s = supply {
    my $r = emit(42);
    say "result: $r";
}
$s.tap({ say "tapped: $_" });
```

```
$ raku                                    $ mutsu
lexical emit called with 42               Use of uninitialized value ...
result: e                                 result:
                                           tapped: 42
```

raku calls the lexical `&emit` (which returns `"e"`, unrelated to the
supply); mutsu instead emits `42` into the supply itself, because
`src/parser/primary/ident/supply.rs`'s `rewrite_supply_body` /
`rewrite_supply_stmt` (and its expression-position twin,
`supply_emit_expr.rs`) match `Expr::Call`/`Stmt::Call` nodes named `emit`
*syntactically*, at parse time, with no check for whether `emit` is a
declared lexical in scope — the same category of bug as the parent ticket,
just in the one caller that still does a purely-syntactic rewrite instead of
deferring to runtime name resolution.

## Why this is harder than the parent ticket

The parent ticket's fix could lean on `is_user_declared_sub` (parse-time
scope tracking) and `amp_binding_in_active_scope` (compile-time local-slot
tracking) because in every fixed case, `emit`/`done` staying an *ordinary*
call was always correct once a lexical was found — normal call resolution
already knows how to prefer a local binding over a builtin.

Inside `supply { ... }`, that is not true: `emit` is deliberately *not* an
ordinary call even when unshadowed — it has to become
`$emitter.emit(ARGS)`, a rewrite that has no runtime equivalent (there is no
builtin "emit" that reaches the right on-demand `Supply::on-demand` emitter
by dynamic scope alone; the whole reason for the rewrite existing is to bind
it lexically to *this* block's emitter at parse time, see the module doc
comment in `supply.rs`). So the fix is not "skip the rewrite" but "skip the
rewrite only for this specific `emit` occurrence, and let normal resolution
find the lexical" — which needs `is_user_declared_sub("emit")` checked at
every one of `rewrite_supply_stmt`'s / `rewrite_expr`'s several match arms
(statement `emit ARGS;`, expression `emit(ARGS)`, the `.emit` topic-method
sugar, and the same set again in `supply_emit_expr.rs` for emit-in-subexpression).
`done`/`ReactDone` has the same shape for its own rewrite site.

## Affected files

- `src/parser/primary/ident/supply.rs` (`rewrite_supply_stmt`, both `emit`
  arms and the `ReactDone` arm)
- `src/parser/primary/ident/supply_emit_expr.rs` (`rewrite_expr`'s `emit` arm)

## Minimal repro

See the snippet above; also reachable via `react { whenever ... { emit(...) }
}` (same rewrite mechanism, different entry point — check `Stmt::Whenever`/
`Stmt::React` handling in `rewrite_supply_body` too).

## Why deferred

This is a real but narrow edge case — a user-declared `&emit`/`&done` lexical
literally named the same as the supply-block sugar, used *inside* the block
that sugar targets. It did not block any roast test at time of writing. The
parent ticket's three concrete repro shapes (top-level lexical, positional
param, named param, all called from *outside* or via a *nested closure* of a
`supply`/`react` block) are fixed; this is the one shape left.
