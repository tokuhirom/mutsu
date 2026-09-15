# Regex: `is_single_regex_atom` now checks the whole atom, not just its edges

`X ** min..max` (a bounded counted-repeat quantifier with no separator) is
lowered by `expand_ltm_pattern`'s string-based LTM expansion: the atom text
immediately before `**` is repeated `min..max` times and wrapped in an
alternation, guarded by `is_single_regex_atom` so this only ever happens to a
genuine single atom (a literal, a character class, a group, a backslash
escape) — never to a multi-token sequence.

`is_single_regex_atom` used to decide "single atom" by checking only the
first and last characters of the candidate text (`'...'`, `[...]`, `(...)`,
`<...>`). The count-spec regex that locates `**min..max` in the pattern is
`^(.+?)\*\*(...)$`, anchored to the *last* `**` in the string — so when an
earlier token in the same pattern also carried its own `**N` quantifier, the
greedy match captured the entire multi-token prefix (spanning past that
earlier `**` too) as "the atom". If that whole prefix happened to start and
end with a quote character — because its first and last tokens were quoted
literals — the old check misclassified the multi-token prefix as a single
bare string literal.

`'x'? \d ** 2 . . 'b' ** 1..3` (issue #8453) hit exactly this: the atom
captured for the trailing `** 1..3` was the entire prefix `'x'?\d**2..'b'`
(starting and ending with `'`), which got string-expanded into an
alternation of that whole prefix text repeated 3, 2 and 1 times. Re-parsing
those repeated copies further corrupted the earlier `\d ** 2` quantifier
(its count-spec scanner consumed the `..` left over from the repetition) and
swallowed the two `.` atoms entirely, producing a spurious "Quantifier range
is empty" abort instead of a real match.

The fix reuses `split_first_atom` — the same balanced-delimiter atom scanner
the separator-splitting code already relies on — to check that consuming
exactly one atom from the front of the candidate text exhausts the whole
string, rather than merely checking its first and last characters. A
regression test, `t/regex/syntax/regex-quantifier-range-atom-boundary.t`,
pins the issue's exact repro plus its narrowing table and confirms the
earlier `\d ** 2` quantifier's own range is unaffected.

Closes #8453.
