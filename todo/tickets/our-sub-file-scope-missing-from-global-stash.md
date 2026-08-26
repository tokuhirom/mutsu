# `our sub` at file scope does not expose `&name` in `GLOBAL::`/`OUR::`

Found while narrowing `GLOBAL::`/`OUR::` stash enumeration (see
`news/2026-08/global-our-stash-enumeration-narrowing.md`). Unrelated to that
fix -- an under-inclusion bug, not the over-inclusion the other entry closes --
and reproduces identically before and after that change.

## Repro

```raku
our sub baz {}
say GLOBAL::.keys.sort;
```

Rakudo: `(&baz)`. mutsu: `()` -- `&baz` is simply absent. A plain `sub foo {}`
or `my sub bar {}` at file scope correctly stay absent too (both are lexical
in raku), so the bug is specific to the `our`-scoped case.

## Where to look

`package_stash_value`'s `registry().functions` scan
(`src/runtime/accessors_stash.rs`), the loop that builds `&name` entries. For
a top-level `our sub`, `stash_member_tail(key, "GLOBAL")` returns the full
registry key unconditionally (same special case as every other GLOBAL lookup
in that function), so the loop *should* see it -- but the resulting `&baz`
entry never appears. Likely causes to check first: the registry key for a
0-arg (non-multi) `our sub` at the top level may carry a different suffix
shape (candidate/signature suffix split on `/`) than the loop expects, or
`is_my_scoped_package_item` may be misclassifying a root-scope `our sub` as
my-scoped. Confirm with a debugger breakpoint on the functions-loop body
(`rust-gdb -batch`, per `CLAUDE.md`'s debugging guidance) rather than guessing
the registry key format.

## Why this is a separate ticket

The over-broad-noise ticket's fix only *removes* wrongly-present members
(builtin classes, dynamic vars, `my` lexicals); it does not touch the
functions loop at all, so it cannot have caused or hidden this. Fixing it is
independent, narrowly scoped to the functions-loop / registry key format, and
should get its own regression test (extend
`t/eval-compunit-introspection.t`'s GLOBAL::/OUR:: section, or a new
`t/*.t`) asserting `GLOBAL::.keys` contains `&baz` for a root-scope `our sub`.
