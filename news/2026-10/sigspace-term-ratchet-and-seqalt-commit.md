# Sigspace whitespace un-ratchets the term before it; a ratcheted `||` commits

In a `rule` (or a ratcheted regex under `:s`), rakudo puts each term's ratchet on the term together
with the significant whitespace that follows it. The whitespace wrapper is what gets ratcheted, so
the term itself can still give back its match. `rule TOP { <b> '!' }` with `regex b { <[x!]>+ }`
parses `x!!`. `rule TOP { <b>'!' }`, with no whitespace after the call, does not. mutsu applied
this rule to quantifiers only. It now applies it to every term that can backtrack: subrule calls,
groups, captures, `|` and `||`. A sigil alias (`$<x>=<b>`) still commits, as in rakudo.

With that rule in place, a ratcheted `||` commits to its first matching branch even when that
branch is zero-width. `token { '(' [ <x>? || <y> ] ')' }` no longer reaches `<y>`, which matches
rakudo. The walk's provisional-zero-width heuristic is deleted. So is the compiled engine's
`seqalt-nullable-ratchet` decline, the most common whole-pattern decline left in ADR-0135 Slice E.

A grammar `method ws { nextsame }` whose deferred built-in fails returns a cursor with a negative
position. That cursor is now a failed call. Before, it counted as a zero-width match.
