# A sigilless bind whose source is another sigilless term takes on its binding

`my \y := $a; my \x := y; x = 5` died with `Cannot modify an immutable Int (1)`
where raku writes `5` through to `$a`. Listed as still open in
`news/2026-08/sigilless-alias-write-now-type-checked.md` and carried in
`todo/tickets/for-loop-sigilless-param-writeback-skips-the-type-check.md`'s
"also still open" section.

The sigiled-target twin (`my $x := y`) has always worked, and so has the
sigiled-source spelling (`my \x := $y`), which localises it precisely: only a
sigilless term on BOTH sides failed.

## Two gaps, one per side of the bind

**The parser's filter never admitted the shape.**
`build_sigilless_bind_stmt` decides between the container-binding statement
shape and the static-readonly one from the RHS: `Expr::Var`, `Expr::Index`,
`Expr::MethodCall`. A sigilless source parses as a bareword, so every such bind
took the readonly path outright. A bareword is now admitted when it is a
declared sigilless value term (`is_user_declared_value_term`) — an ordinary
bareword (a type name, a listop call) still takes the readonly path, and the
runtime settles writability for the ones that are admitted.

**The store resolved the source by name.** With the shape admitted, the source
reaches the store as a `WrapVarRef` tag, and the bind path resolves it through
the `__mutsu_sigilless_alias::` chain. That chain only ever records a link to a
NAMED variable, so:

- an element alias (`my \e := @a[0]; my \f := e`) has no entry in it and the
  write landed in a copy — a silent drop;
- a value binding (`my \lit := 5; my \z := lit`) looked writable, because the
  tag named a real variable.

`OpCode::MarkSigillessBindSource` (added the same day, and emitted only by a
sigilless declaration bind) now closes both from the one place that sees the
source: when the tag carries a real slot whose RAW local is a `ContainerRef`, it
hands that cell to the store instead of the tag — a sigilless term's slot holds
exactly the binding it took, and `GetLocal` had merely deref'd it for the read.
And a tag naming a term that itself carries the
`__mutsu_sigilless_readonly::` marker is not writable; a sigiled source never
carries that marker, so the ordinary `my \x := $a` case is untouched.

## Coverage

`t/sigilless-bind-chain.t` — 14 assertions, all dual-oracled against raku: two-
and four-hop chains through a named variable (write-through and live read),
chains through array- and hash-element aliases, chains rooted at a literal, a
`constant` and a type name (all still immutable, with the message raku gives),
and the sigiled-target control. `t/bind-alias-is-a-container.t` (34),
`t/sigilless-bind-writability-source.t` (16), `make test` (3644 files) and a
282-file targeted roast sweep are green.
