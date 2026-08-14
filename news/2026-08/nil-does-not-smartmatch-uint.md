# `Nil` no longer smart-matches `UInt`

`Mathematica::Serializer::Encoder`'s own test suite (a triaged row in
`todo/tickets/dist-test-suite-failures-batch.md`) had one near-miss: its
`given $obj { ... when UInt {...}; when $_.isa(Any) { self.Nil-to-WL() } }`
dispatch serialized a `Nil` pair value as an empty string instead of
`NULL`, because `Nil ~~ UInt` returned `True` in mutsu (raku: `False`), so
the `when UInt` branch matched instead of falling through to the `Any`
branch that produces `NULL`.

Root cause: `type_matching.rs`'s `UInt` constraint branch had a stray
`ValueView::Nil => true` arm, added in #851 to let `$u = Nil` reset a
`UInt`-typed variable to its default. That specific case is already handled
generically, *before* `type_matches_value` is ever called for it — the
`TypeCheck` opcode skips the whole check with a `!value.is_nil()` guard
(`vm_misc_typecheck.rs`) — so the arm was dead weight for its original
purpose and only surfaced as this `~~`/`given`-`when` regression. Removed;
`roast/S32-num/int.t` (#851's original motivating test) still passes
165/165. Pinned by `t/nil-uint-smartmatch.t`.
`Mathematica::Serializer::Encoder`'s suite now passes 3/3, matching raku.
