# The compiled regex engine runs the whole capture language

Slice A of ADR-0135 (#10251) is complete. The compiled regex engine now runs every capture form
of the regular core. Before this, each of the following sent its pattern back to the tree walk:

- nested capture groups, as in `((a)(b))` and `((a)(b))+`;
- backreferences, as in `(\w+) $0` and `$<q>=. X $<q>`;
- the `<(` and `)>` match-boundary markers;
- quantified aliases, as in `$<x>=(\d)+`;
- captures under a separated quantifier, as in `(\w)+ % ','` and `[ $<k>=\w '=' $<v>=\d ]+ % ';'`.

A capture group whose body captures now gets a capture level of its own. Its captures number from
zero and become the group's sub-Match when it closes. Backtracking can re-enter a group after it has
closed, so the engine journals every change to its stack of levels. A choice point then only has to
record one journal length. A separated quantifier matches each atom and each separator in its own
level. At the end it folds them side by side through the same helper the walk uses.

The differential mode (`MUTSU_RX_DIFF=1`) found one more bug in the walk, now fixed to match
rakudo. A backreference inside `[ … ]` numbered from the group's own captures, so
`"aba" ~~ / (a) [ (b) $0 ] /` failed. Rakudo matches it, because `$0` is the `a`.

`scripts/rx-decline-survey.sh` is new. It runs all of `t/` and the roast whitelist under
`MUTSU_VM_STATS=1` and sums the reasons each pattern stayed on the walk. That sum is ADR-0135's
migration ratchet. Across that corpus, compiled patterns went from 5,154 to 5,290 and declined
ones from 2,149 to 2,007. The Slice A reasons are gone: `backref`, `capture-marker`,
`nested-capture`, `quantified-nested-capture`, `quantified-alias`, `separator-capture` and
`alias-form`. Most of `other-atom` went too, since `<?same>` and `<at(N)>` now compile. What is
left is `<~~>` (`recurse-self`), which belongs to the subrule slice.
