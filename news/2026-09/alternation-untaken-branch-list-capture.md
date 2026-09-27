# An alternation's untaken branch now leaves list-valued captures as `[]`, not `Nil`

Found while working on ANTLR4::Grammar (#9491): `rule lexerAlt { <lexerElement>+ | '' }`
matching the `''` branch left `$/<lexerElement>` as `Nil` instead of `[]`, so the dist's
action (`my @child = $/<lexerElement>>>.ast`) hit a `Nil` where it expected an empty list.

Rakudo decides list-vs-singular for a capture NAME (or positional group) statically, from
the whole regex — QRegex's `capnames` analysis: quantified, or bound more than once in the
same sequence (`<e> <e>`), makes it list-valued everywhere in the pattern, whether or not
the branch that would populate it ever ran. mutsu instead only seeded the empty-list default
at the zero-match point of each quantifier (`(a)*` matching zero times, or a zero-matched
`?`), which never runs for a branch an alternation's cursor never entered at all.

`alternation_list_flags` (`runtime/regex/regex_helpers.rs`) now computes this once per
alternation atom: a `NameMult` lattice (`None`/`One`/`Many`) folds a name's occurrences
through a sequence (two single occurrences combine to `Many`) and across branches (the max
wins), mirroring the positional side's existing "widest branch" rule
(`capture_group_list_flags`). `alternation_branch_delta` (`runtime/regex/regex_match_delta.rs`)
is now the one place every alternation-branch transform pads an untaken branch's captures —
six call sites across `regex_match_atom.rs`, `regex_match_capture.rs` and
`regex_match_lazy.rs` duplicated the same resize-and-merge logic, and only one of them padded
positional slots at all; none seeded named captures.

Pinned by `t/regex/regex-alternation-untaken-branch-list-capture.t`.
