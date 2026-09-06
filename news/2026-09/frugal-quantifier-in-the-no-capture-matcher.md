# The no-capture regex matcher ignored frugal quantifiers — and that is what closed `Template6`

`Template6` 0.16.0 reached **12/12** upstream test files on 2026-09-06, which
closes the dist and gives the template-battery survey
(`docs/batteries/templates.md`) a genuine second option instead of a single
viable choice. The last failing file, `t/05-includes.rakutest`, turned out to be
one general interpreter bug in a subsystem the ticket had not suspected.

## The reduction

`todo/tickets/template6-include-local-data-not-reaching-the-included-stash.md`
recorded the symptom precisely — `[% INCLUDE "included" name = "World" %]`
rendered `<h1>Hello name</h1>` instead of `<h1>Hello World</h1>` — and then
guessed at the mechanism: the `|%localdata` flatten into a `*%params` slurpy,
`Stash.make-clone`, and `Parser.compile`'s `:nd(2..*)` substitution. All three
guesses were wrong, and the ticket's own last suggestion was the one that
worked: uncomment `Parser.compile`'s `note "<DEBUG:template>..."` and diff the
generated script against raku's. The diff was two lines long — mutsu's generated
script simply had no `%localdata<name> = 'World';` line at all.

Instrumenting `parse-template` showed why: raku hands it
`["\"included\"", "name", "=", "\"World\""]` and mutsu hands it one element,
`["\"included\" name = \"World\""]`. The directive tokenizer is

```raku
.comb(/ \" .*? \" | \' .*? \' | \S+ /)
```

and that reduces in one step to a two-line repro with no module in sight:

```raku
say 'aXbXc'.comb(/a.*?X/).raku;   # raku: ("aX",)   mutsu: ("aXbX",)
```

`~~` and `.match(:g)` were both frugal on the same pattern. Only `.comb` was
greedy.

## Root cause

mutsu has two regex matchers: the capturing one (`regex_match_core.rs`, used by
`~~`, `.match`, grammars) and a no-capture one
(`regex_match_nocap.rs`'s `regex_match_end_from_in_pkg`) that only computes match
*extents*. The second is what `regex_find_all` drives, and therefore what
`.comb(/rx/)`, `.split(/rx/)`, `.subst(..., :g)` and the `<?before ...>` /
`<?after ...>` assertions the capturing matcher delegates to all run on.

That matcher never read `RegexToken::frugal`. It explores candidate end
positions through a LIFO stack, and it built each quantifier's candidate list in
ascending order and pushed it unchanged — so the longest repetition was always
popped first. That is exactly greedy priority, applied to greedy and frugal
quantifiers alike. `.comb(/ \" .*? \" /)` therefore matched from the first quote
to the *last* one, swallowing everything between two quoted runs, and the
`Template6` statement `"included" name = "World"` came back as a single token.

## The fix

`regex_match_nocap.rs` now mirrors the capturing matcher's priority rule. A new
`push_quant_positions` helper pushes a quantifier's candidate positions in
descending order when the token is frugal (shortest explored first) and in
ascending order otherwise, and the `?` / `??` arm chooses between the
zero-match and one-match branches the same way. `One` is unaffected.

The fix is at the shared primitive, so every consumer of the no-capture matcher
gets it at once — including a frugal quantifier written inside a lookahead
assertion, which had the same defect through a different door.

## Pins

`t/regex-frugal-quantifier-nocap-matcher.t`, 16 assertions, byte-identical under
`mutsu` and `raku`: `.comb`, `.split`, `.subst(:g)`, `~~` and `.match(:g)` on the
same frugal pattern, each greedy counterpart alongside it so the fix cannot be
satisfied by making everything frugal, `??` vs `?`, a frugal `**` range that
still has to grow to satisfy what follows, and a frugal quantifier inside a
`<?before ...>` assertion.
