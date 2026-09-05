# Ordered alternation is still eager inside a non-ratcheted subrule

`news/2026-09/ordered-alternation-driven-by-the-continuation.md` moved `||` off
the eager atom producer and onto the token walk, so branch *k+1* is evaluated
only after branch *k*'s candidates have been rejected by the real continuation.
That fixes the ordered-alternation semantics wherever the walk is the *committed*
driver: a `~~` match, a `token`/`rule` (ratchet commits to the first matching
branch), `Grammar.parse` of the start rule (which now stops at the first full
match), and any alternation whose continuation lives in the same pattern.

It does **not** fix the one context where mutsu asks a pattern for its whole
candidate *set* instead of for one committed match: a **non-ratcheted** (`regex`)
subrule. There the walk keeps going after an earlier branch has already produced
a complete match, so a later branch's `{ … }` block runs when raku's cursor would
never have entered it.

## Repro (measured 2026-09-05 against `raku`)

```raku
my @j;
grammar GJ {
    regex TOP { <part> 'cd' }
    regex part { 'a' [ 'b' { @j.push('one') } || 'bc' { @j.push('two') } ] }
}
say GJ.parse('abcd').defined;   # both True
say @j.raku;                    # raku: ["one"]   mutsu: ["one", "two"]
```

This is over-firing, not under-firing: the match itself is correct, and every
block mutsu runs is one raku *would* run if the continuation had failed. It only
matters for a block with a side effect that must not happen — the shape that
motivated the original `SPECULATIVE_ALT_BRANCH` band-aid was exactly that
(`Config::TOML`'s `|| . { die "bad escape sequence" }`), though that grammar is
written with `token`, so it is ratcheted and is not affected. Every bundled
battery's grammars are `token`/`rule` too; this needs a hand-written `regex`
subrule with a side-effecting `||` branch to bite.

## Why the subrule enumerates

`<part>` is matched by asking `part`'s pattern for every end it can reach, so the
caller can backtrack into it. raku really does re-enter such a subrule
(`regex part { 'a' [ 'bc' || 'b' { … } ] }` under `TOP { <part> 'cd' }` reaches
the second branch, and mutsu now reaches it too), so the candidate set cannot be
truncated the way the ratcheted case can — `regex r { <?> || x <r> }` needs every
one of its ends for the enclosing anchor to find the right one.

Making this exact requires the subrule boundary to be continuation-driven rather
than collect-then-pick: the caller's continuation would have to be handed to the
callee's walk.

## Why it is large

mutsu's atom producers return `Vec<(usize, RegexCaptures)>` and every caller
consumes that vector, so there is no continuation to hand down. Turning that into
a resumable / CPS interface touches `regex_match_atom.rs`,
`regex_match_atom_simple.rs`, `regex_match_capture.rs`, `regex_match_sep.rs` and
the quantifier walks in `regex_match_core.rs` — the hottest path in the engine.
It should be scoped and measured as its own campaign, with an ADR for the
interface change, rather than bolted onto a bug fix.

A cheaper partial step, if the full campaign is not worth it: give
`regex_match_ends_from_caps_in_pkg` a "the caller only wants ends up to length N"
or "the caller is itself ratcheted" hint, so a subrule called from a ratcheted
token — which cannot be backtracked into anyway — is walked with `first_only`.
That would not help the repro above (both rules are `regex`), but it would cut a
lot of wasted enumeration on the common grammar path; measure before assuming it
is free.
