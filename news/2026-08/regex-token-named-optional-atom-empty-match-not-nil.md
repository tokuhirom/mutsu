# A `?`-quantified named token now yields an empty Match, not Nil, when it takes zero reps

Found via `Template::Mojo` 0.2.2 (`t/03-capture.rakutest`,
`todo/tickets/template-mojo-residual-failures.md`), whose `perlline` grammar
token is `^^ \h* '%' $<get-result>=['=']? $<expr>=[...] [\n | $]`. When the
optional `=` did not appear, mutsu's `$<get-result>` was `Nil`; raku's is a
defined, empty `Match`:

```raku
if "b" ~~ / $<x>=[<[cd]>]? "b" / {
    say $<x>.WHAT;   # raku: (Match)   mutsu (before): Nil
}
```

Whether an unmatched optional named capture is `Nil` or an empty `Match`
turns out to depend on *where* the `?` sits, verified case-by-case against
`raku`:

- `$<x>=[...]?` / `$<x>=<[...]>?` — the `?` quantifies the same token the
  name is attached to. The token "runs" as a unit even on its zero branch, so
  `$<x>` is a defined, zero-width `Match`.
- `(x)?` / `$<x>=(...)?` — the `?` quantifies a `CaptureGroup` atom. Raku
  still yields `Nil` here even when the capturing group itself carries the
  name — the capturing-group case is special-cased differently from every
  other atom kind.

mutsu's regex engine (`src/runtime/regex/regex_match_core.rs`,
`RegexQuant::ZeroOrOne`) has three "zero repetitions won" branches (ratchet,
frugal, and the default greedy fallback), and none of them applied the
token's own `$<name>=` alias — only the loop over actually-matched candidates
did. Fixed by calling the existing `store_apply_named_capture` helper with a
zero-width span (`pos, pos`) in all three zero branches, gated on the atom
not being a `CaptureGroup` (which keeps the `(x)?`/`$<x>=(...)?` case `Nil`,
matching raku). `store_apply_named_capture` was already a no-op for tokens
with no `named_capture`, so this is safe to call unconditionally for
non-`CaptureGroup` atoms.

Chasing this down the `Template::Mojo` test surfaced a second, unrelated
pre-existing bug: `$<name>` for a capture *absent from the current match's
own pattern* was falling back to a stale value from an earlier match in the
same scope instead of `Nil`. That is now tracked separately —
`todo/tickets/named-capture-absent-from-current-match-leaks-stale-value.md` —
since it's a different lookup path (this fix is about names *present* in the
pattern that matched zero times, not names absent from the pattern
entirely).

Regression test: `t/regex-optional-named-capture-nil-vs-match.t`.
