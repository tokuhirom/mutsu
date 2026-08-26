# `Str.comb(:match)` dispatches — the ticket was closed by the implicit `*%_` fix

`Type/Str.rakudoc:647` reported that `"abc".comb(:match)` — a call with only a
named argument and no positional matcher — failed to dispatch at all, dying with
`No such method 'comb' for invocant of type 'Str'` while every other arity of
`.comb` worked.

Re-measured on current `main`, it no longer reproduces. The cause was exactly the
one [`news/2026-08/native-methods-honour-the-implicit-slurpy-named.md`](native-methods-honour-the-implicit-slurpy-named.md)
diagnosed and fixed: native methods are selected by arity, the named `Pair`
counted as a positional, and the lookup for a one-positional `comb` missed the
zero-positional arm sitting right there. `call_method_with_values` now offers the
full argument list first and retries with the nameds removed when — and only
when — the whole chain answers `X::Method::NotFound`, which is precisely this
shape. That same PR also taught `native_comb_method` and `dispatch_comb_with_args`
to drop an unrecognized named-flavour `Pair` rather than read it as the matcher.

So `.comb` did **not** need the per-method accepted-named declaration that
`todo/deep/native-method-accepted-named-declarations.md` describes; it was on the
loud side of that split, which the retry mechanism already covers soundly. That
deep ticket stays open for the silent half (`.chop(:zzz)`, `.polymod(3, :zzz)`,
`.fmt("%d", :zzz)`, `.rotor`, `.classify`, `.first`), where the wrong arm *hits*
and there is no error to retry on.

## What the ticket got wrong about the expected output

The ticket recorded raku's answer for `"abc".comb(:match).raku` as
`(｢a｣ ｢b｣ ｢c｣)`. It is not: with no matcher there is nothing to make a `Match`
out of, and raku v2026.06 returns `("a", "b", "c").Seq` — plain `Str`s, confirmed
with `.map(*.^name)`. `:match` only produces `Match` objects when a matcher is
supplied (`"abc123def".comb(/\d/, :match)`), which mutsu already did correctly.

Closed with regression coverage in `t/str-coercion-and-dispatch.t`, which pins
all four shapes (`.comb`, `.comb(:match)`, `.comb(:match, $limit)`,
`.comb($matcher, :match)`) so the named-only dispatch cannot silently regress.
