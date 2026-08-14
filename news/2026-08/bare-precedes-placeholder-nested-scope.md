# `$^name` in a nested `if`/`for`/`given` block no longer shadows an outer bare `$name`

A placeholder parameter (`$^name`) declares its block's `$name` under the
*plain* name, so a bare `$name` written where no reachable scope declares it
is `X::Undeclared` in rakudo. mutsu's `bare_precedes_placeholder` implemented
the *same-block* half of this rule (a bare `$name` used before the `$^name`
that declares it, in the same block), but had no notion of *nested* scope: a
`$^name` used only inside a strictly nested `if`/`for`/`given` **block** body
(or a separate closure) does NOT make `$name` the *enclosing* block's
parameter — that inner block owns it — yet mutsu let a bare `$name` used in
the enclosing block resolve anyway, executing on a stale/leaked value instead
of raising a compile-time diagnostic:

```raku
my $f = { for 1 { $^b }; say $b }; $f(42)   # rakudo: X::Undeclared; mutsu (before): printed 1
my $f = { if 1  { $^b }; say $b }; $f(42)   # rakudo: X::Undeclared; mutsu (before): printed 1
```

Root cause: the compiler's placeholder-conflict check
(`check_placeholder_conflicts` in `compiler/helpers_call_args.rs`) only ever
ran the ordering check against a block's *own* placeholder list (from
`collect_placeholders_shallow`, which correctly excludes a nested block's
placeholders). Since `$^b` in the examples above belongs to the inner
`for`/`if` block, it never appeared in the outer block's own placeholder
list, so the ordering check was never even invoked for `b` — the bare `$b`
silently fell through as an ordinary (and here, undeclared-but-unchecked)
variable reference.

Fixed by extracting the ordering/scope-check family out of `ast.rs` into a
new `src/placeholder_order.rs` module:

- `check_bare_var_stmt` / `check_bare_var_expr` (private walkers backing
  `bare_precedes_placeholder`) were rewritten to mirror
  `collect_ph_stmt_shallow` / `collect_ph_expr_shallow`'s scope-boundary
  decisions exactly — descending through statement headers, `while`/`loop`/
  block-style non-boundary bodies, and statement-*modifier* bodies, but
  stopping at every construct that opens its own placeholder scope
  (`if`/`for`/`given` BLOCK bodies, `whenever`, closures) — instead of the
  previous ad-hoc, much more limited set of statement arms.
- A new function, `bare_name_shadowed_by_nested_placeholder`, finds a bare
  name referenced in a block's own placeholder scope whose only matching
  `$^name` placeholder lives in a block strictly nested inside it (via the
  existing deep `collect_placeholders`, filtered against the block's own
  shallow placeholder list). `check_placeholder_conflicts` now also runs this
  check, raising the same generic `X::Undeclared` rakudo gives (it does not
  mention `$^name` in the message — the nested placeholder is not actually
  relevant to why `$name` is undeclared, it just happens to be the same
  spelling), unless `$name` is otherwise declared: via `my`, an outer scope,
  or as the block's own (non-placeholder) signature parameter — the last of
  which was needed to avoid a regression in
  `t/placeholder-nested-block-scope.t`'s "bitwise placeholder blocks, slipped
  arguments" case (`-> $b, $i { ({ $^a +& $^b ... }, ...)[$i](|$b) }`, where
  `$b` is the pointy block's own declared parameter, unrelated to the nested
  closures' own `$^b`).

The `for` statement **modifier** case remains legal and unaffected (a
modifier body is not a block; see
`news/2026-08/for-modifier-placeholder-scope.md`).

Pinned by `t/bare-precedes-placeholder-nested-scope.t` (9 assertions: the two
repro cases, an analogous `given` case, the statement-modifier case staying
legal, `my`/outer-declared exemptions, and a separate-closure case).

Not fixed in this change (tracked separately):

- `todo/tickets/bare-precedes-placeholder-same-statement-order.md` — the
  ordering check is still tracked per *statement*, not per sub-expression, so
  `{ $b + $^b }` is still accepted even though the bare use precedes the
  placeholder within the one statement.
- `todo/tickets/placeholder-scope-while-loop-not-a-boundary.md` — rakudo
  treats `while`/`loop`-style bodies as their own placeholder scope too
  (`{ while True { $^c; last }; say $c }` is `X::Undeclared` in rakudo), but
  `collect_placeholders_shallow` (and this fix, which mirrors it) does not
  treat them as a boundary, so that case is still accepted by mutsu.
