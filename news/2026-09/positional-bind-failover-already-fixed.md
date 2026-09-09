# `does PositionalBindFailover` binding already fixed

Issue #7751 reported that a class composing `PositionalBindFailover` could not
bind to an `@`-sigilled parameter: mutsu rejected it with a plain
`Positional` type check instead of falling back to the object's `.iterator`,
as Raku requires.

Investigating the issue found that `check_and_coerce_param_type` in
`src/runtime/types/binding_signature.rs` already special-cases
`PositionalBindFailover` (and the built-in `Seq` failover) by calling
`coerce_positional_bind_failover`, which asks the value for its `.iterator`
and binds the reified list. This landed as part of `fix: preserve runtime
binding failures` (fb90c40), together with a regression test,
`t/issue-7750-runtime-binding-errors.t`, that pins exactly the scenario from
#7751:

- `first-five(FallbackSource.new)` binds through the custom `.iterator` and
  reads back `(42, Nil, Nil, Nil, Nil)`.
- `first-five((1, 2).Seq)` binds the built-in `Seq` failover the same way.

Both match Rakudo's output. Slurpy `*@a` binding was also checked against
Rakudo: a slurpy parameter captures the raw argument list without consulting
`PositionalBindFailover` in either implementation, so no further change was
needed there.

No code change was required; issue #7751 was closed as already resolved.
