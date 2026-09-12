# The listop invocant colon dispatches a method instead of being dropped

Raku's listop invocant colon makes the first argument the *invocant*: `foo $x: @args`
is `$x.foo(@args)`. With nothing after the colon, `die "boom":` is therefore
`"boom".die` — and since `Str` has no `die` method, rakudo raises
`X::Method::NotFound`.

mutsu implemented that rule for the generic no-paren listop path
(`try_parse_no_paren_invocant_colon_call`) and for the parenthesized spelling
`foo($x:)`, but three places bypassed it, each in a different way:

* **The dedicated statement parsers.** `die`/`fail`/`take`/`take-rw` consume their
  argument with a plain `expression` call, which stops *before* the colon and left
  it unconsumed — so `die "boom":` did not parse at all. `return $x:` had a
  targeted patch that consumed a bare trailing colon and threw it away.
* **The tail-`Stmt::Call` value path.** When a `Stmt::Call` is the last statement of
  a sub body, its arguments are remapped to expression arguments by
  `call_args_to_expr_args`, which flattens `CallArg::Invocant(e)` to a plain
  positional. `sub g() { warn "w": }` therefore *warned* where rakudo throws.
* **`compile_tail_stmt_call_value`.** The other tail path did not flatten the
  invocant — it fell through to the named/slip branch and hit its `unreachable!()`,
  so `warn "w":` as the program's final statement panicked the compiler.

The visible damage was graded. `say "hello":`, `sort @a:` and `return $x:` agreed
with rakudo *by coincidence* — dropping the colon and dispatching the method happen
to produce the same result there. `warn "w":` was a live wrong answer. `die "boom":`
was a parse error, which is what blocked `Math::FractionalPart` (and through it
`Astro::Utils` and `DateTime::Julian`) from loading at all.

Fixing only `die`'s parse would have traded the loud failure for a quiet one: the
module would load and then silently `die "FATAL: ..."` where rakudo raises
`X::Method::NotFound`. So the shared rule was fixed instead.

## What changed

* `control_stmts.rs` gains `try_invocant_colon_stmt`, a thin wrapper over the same
  `try_parse_no_paren_invocant_colon_call` every other listop already uses.
  `die`/`fail`/`take`/`take-rw`/`return` each consult it after parsing their
  argument, so all five now build the method call rather than dropping the colon
  (`return`'s bespoke bare-colon patch is gone).
* `Compiler::invocant_colon_method_call` is the one place that turns a
  `Vec<CallArg>` carrying a leading `CallArg::Invocant` into the equivalent
  `Expr::MethodCall`. `compile_stmt`'s `Stmt::Call` arm (which had this inline),
  `compile_tail_stmt_call_value`, and the sub-body tail-`Stmt::Call` arm all route
  through it, so no compile path can silently flatten an invocant again.
  `call_args_to_expr_args` documents that callers must rule out the invocant first.

All five listops from the issue's survey table now match rakudo, and they match it
for the right reason. `return $x:` keeps its control transfer and its return-type
check, because `Mu.return` goes through the routine's return path exactly as
`return $x` does.

`t/routines/call/call-listop-invocant-colon.t` pins the whole family — the listops
whose method exists (`sort`, `return`, `take`), the ones whose method does not
(`warn`, `die`, `fail`, `take-rw`), both the mid-body and final-statement compile
paths, argument passing after the colon, and the two things that are *not* invocant
colons (a leading colonpair adverb, and the `::` of a type name). It passes under
rakudo unchanged.

Closes #8141.
