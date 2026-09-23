# A declarative-prefix stopper in one `|` branch could truncate its sibling's own measurement

Investigating #9053 (a CSS::Module::CSS3::Selectors regex-alternation mismatch)
turned up a real, independently-fixable bug in the ADR-0022 LTM ranking
machinery, though it does not fully explain that issue's reported symptom
(which stays open, re-triaged with a much smaller reduction).

Both `Alternation` arms that rank a `|`'s branches — the plural one in
`regex_match_atom.rs` (`ltm_rank_and_collect_branches`) and the singular one
in `regex_match_capture.rs` (used for `[a|b]+` quantifier growth) — loop over
every alternative and call straight into the real matcher to check whether
each one matches at all. That call is *not* wrapped by `ltm_prefix_len_at`'s
own save/restore of `LTM_PREFIX_TERMINATED` (ADR-0022's "this branch's own
walk hit a non-declarative stopper" flag) — it is a direct real-match check,
used regardless of whether `LTM_DECLARATIVE_MODE` happens to be ambient at
the time.

When one of these `Alternation` arms is invoked while `LTM_DECLARATIVE_MODE`
is *already* ambient — because this whole alternation is itself nested
inside an ancestor's own `ltm_prefix_len_at` measurement, such as a proto
candidate's body — a branch that hits a stopper (a `<.ws>`, a code block, …)
left `LTM_PREFIX_TERMINATED` set to `true` after its own walk. Nothing reset
it before the *next* sibling alternative's own walk began, so the stopper
leaked sideways: `walk_tokens`'s entry check fires whenever the flag is
already set, before comparing a single atom, and accepted a bogus zero-width
"match" for the sibling — even when that sibling's real body could not
possibly match at that position.

Concretely, a proto with a candidate whose body contains a nested,
unquantified `[ <.ws> | 'zz' ]` alternation had its own declarative prefix
undercounted: `<.ws>` (declared first) set the flag, and the leak then made
the immediately-following `'zz'` branch measure as a zero-width match too,
so the candidate's true prefix (`'zz''X'`, length 3) measured as 0 instead —
letting a shorter, unrelated candidate win the proto ranking outright.

Fixed by restoring the loop's starting `LTM_PREFIX_TERMINATED` value before
every alternative's own match/measurement attempt, in both arms, so a
stopper hit while evaluating one branch can no longer bleed into a sibling.
Regression: `t/regex/regex-ltm-sibling-termination-leak.t`.

`roast/S05-metasyntax/longest-alternative.t` stays 62/62.
