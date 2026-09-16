# `:i` no longer widens a named built-in or Unicode property class

Under `:i`, a composite regex class combining a named built-in class with
another item (`<+upper -[A]>`), a bare named built-in class (`<lower>`, which
parses through the same composite-class machinery), or a Unicode property
inside a composite class (`<+:Lu>`) incorrectly matched *any case variant* of
a member character instead of only its literal members. For example,
`"a" ~~ / :i <upper> /` matched, even though `a` is not an uppercase
character — only its uppercase fold (`A`) is a member of `<upper>`.

## Root cause

`:i` in Raku governs literal character/range comparison, not membership of a
named class or Unicode property: `<upper>` still means "an uppercase
character" under `:i`, not "any case variant of an uppercase character".

mutsu's two class-membership predicates — `class_matches_ignorecase` (plain
classes) and `composite_item_matches` (composite `<+a -b>` classes, shared
with the ADR-0099 Stage 1 scan prefilter) — both folded the *subject
character* first via `CaseFoldIter` and then tested class membership on every
fold variant, for the whole class uniformly. That conflated the two distinct
`:i` behaviors: a literal or range item is correctly meant to fold, but a
named builtin or Unicode property item is not.

Fixed by testing each class item individually: `ClassItem::NamedBuiltin` and
`ClassItem::UnicodePropItem` are tested against the subject's own,
unfolded character only, while every other item kind (`Char`, `Range`, etc.)
still folds as before. This also fixes a negated named builtin
(`<-lower>`) inside a composite class, and a class that mixes a literal with
a named builtin (only the literal half folds).

Found while deriving the ADR-0099 Stage 1 scan-prefilter's first sets; not a
prefilter-only bug, since it reproduced identically with
`MUTSU_REGEX_PREFILTER=off` — both engine functions shared the same
fold-before-test shape.

Pinned in `t/regex/regex-ignorecase-named-builtin-no-fold.t`.
