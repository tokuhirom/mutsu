# `given`/`with` scalar pointy params now enforce readonly without `is rw`

`given`/`with EXPR -> $v {...}` (no `is rw`) previously allowed `$v = ...`
silently — raku dies with "Cannot assign to a readonly variable" instead,
regardless of whether the topic source itself is mutable (`given 42 -> $v`
and `given $x -> $v` both die the same way; `@`/`%`-sigil pointy params stay
writable unconditionally, no trait needed).

The fix threads the same `Stmt::MarkReadonly` mechanism already used for
`for`-loop pointy params (`for @a -> $x { ... }`) into `pointy_topic_bind`
(`src/parser/stmt/control.rs`): a scalar (`$`, non-sigilless) pointy param
without an `is rw` trait gets a `MarkReadonly` statement appended after its
synthetic bind declaration, for both the native-typed and general branches.

That alone did not fully work, though: the readonly mark was set but never
consulted. `compile_when_tail_stmt`'s `Stmt::Assign` arm
(`src/compiler/helpers_block_inline.rs`) — which special-cases the LAST
statement of a `given`/`when`/`with` block body so its value becomes the
block's value — compiled straight to `compile_expr` + `Dup` +
`emit_set_named_var`, entirely bypassing the `OpCode::CheckReadOnly` emission
the general `Stmt::Assign` compile arm always does. This was a broader,
pre-existing gap: ANY tail-position assignment in a `given`/`when`/`with`
block skipped the readonly check, not just pointy params (confirmed by
testing a readonly topic assignment, `given @a { $_ = 5 }`, at tail position
before the fix — it wrongly succeeded). Fixed by emitting the same
`CheckReadOnly` op there too, matching the general arm's ordering (after the
RHS is evaluated, mirroring raku's own evaluate-then-reject order, verified
against `raku -e`).

Pin: `t/given-with-pointy-scalar-readonly-enforcement.t` (readonly-without-rw
across plain/tail-position/literal-topic/`with`/native-typed shapes, plus
`@`/`%`-sigil unconditional writability). One pre-existing local test,
`t/given-with-scalar-pointy-rw-writeback.t`, had a case exercising `given 5
-> $x { $x += 100 }` with no `is rw` — that was itself relying on the bug
(raku also dies there); updated to add `is rw` and a proper lvalue topic
(`is rw` on a literal topic dies differently in raku — "expects a writable
container" — a separate, uninteresting-to-pin gap).

Found and filed separately, not fixed here: native-typed pointy params
(`given $x -> int $v is rw { ... }`) don't write back at all yet — confirmed
pre-existing on `main` via `git stash`, unrelated to this fix. See
`todo/tickets/native-pointy-param-is-rw-writeback-missing.md`.
