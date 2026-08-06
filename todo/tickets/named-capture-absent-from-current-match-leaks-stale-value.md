# `$<name>` for a capture absent from the current match leaks the previous match's value

Found while writing a regression test for
`news/2026-08/regex-token-named-optional-atom-empty-match-not-nil.md`
(the `?`-on-the-named-token fix).

`$<name>` (equivalently `$/<name>` / `$<name>` after `~~`) should be `Nil` when
the *current* successful match's pattern has no capture called `name` at all
-- not merely when the name matched zero times. mutsu instead returns the
value from an *earlier* match in the same dynamic scope:

```raku
if "xb" ~~ / $<x>=<[cdx]> "b" / {
    say "block1 x=", ~$<x>;      # x           -- both agree
}
if "bb" ~~ / "b" "b" / {         # this pattern has no $<x> at all
    say "block2 x=", $<x>.WHAT;  # raku: Nil : mutsu: (Match), the stale "x" from block1
}
```

raku: `block1 x=x` / `block2 x=Nil`.
mutsu: `block1 x=x` / `block2 x=(Match)` (leaking block1's captured Match).

Reproduces the same way through a plain top-level `sub` call, so it is not
specific to bare-block topicalization:

```raku
sub m1 { "xb" ~~ / $<x>=<[cdx]> "b" / }
sub m2 { "bb" ~~ / "b" "b" / }
m1();
m2();
say $<x>.WHAT;   # raku: Nil, mutsu: (Match)
```

## Why this matters

Any code that branches on `$<name>.defined` after a *second* match whose
pattern doesn't declare `name` will silently see a stale Match object instead
of Nil. This is a narrower, unrelated bug from the "which `?` placement gives
Nil vs an empty Match" question that
`news/2026-08/regex-token-named-optional-atom-empty-match-not-nil.md` fixed --
that fix is specifically about names *present* in the current pattern that
matched zero times; this ticket is about names *absent* from the current
pattern's token list entirely.

## Where to look

The named-capture read path for `$<name>` / `$/<name>` (`Match` indexing by
Str key) almost certainly resolves through some `$/`-adjacent lookup that,
when the current match's `named` map has no entry for the key, falls back to
a broader/older store instead of returning `Nil` outright. Candidates:
`src/runtime/regex/regex_match_public.rs`, the `Match` hash-subscript method
implementation, and wherever the interpreter installs `$/` after a successful
top-level `~~`/`.match` (look for where the *previous* `$/`'s named map could
still be reachable when building the new one).

## Effort

Not measured; likely S-M once the actual lookup site is found, but requires
tracing the `$/`-installation path fresh (not touched by the ticket that
found this).
