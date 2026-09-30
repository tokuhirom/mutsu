# The compiled regex engine runs `|` alternation

Slice B of ADR-0135 (#10252) begins with longest-token alternation. A `|` used to send its whole
pattern back to the tree walk. Now it compiles to one `LtmAlt` op. At the current position the op
ranks the branches with the walk's own LTM key (`ltm_branch_rank_key`), so ADR-0022, ADR-0046 and
ADR-0127 still decide the ranking. It then enters the best branch and leaves the others on the
backtrack stack, the next-best on top.

A lower-ranked branch is therefore entered only after every branch above it has failed against
the rest of the pattern. #9922 asked for exactly that: a losing branch's code runs only if the
winner fails. The regression test, `t/regex/regex-ltm-losing-branch-code.t`, pins the order
against rakudo:

```raku
my @log;
"ab" ~~ / [ a { @log.push: "A" } | ab { @log.push: "AB" } ] b /;
say @log;   # [AB A]: `ab` wins, `b` then fails, and only then does `a` run
```

The differential mode caught one bug before it landed. A `|` nested inside another `|` reused the
outer one's branch table, because the table was numbered after its branches were compiled.

Across all of `t/` and the roast whitelist (`scripts/rx-decline-survey.sh`), compiled patterns
went from 5,290 to 5,537, and declined ones from 2,007 to 1,792. The `alternation` reason is gone.
A failing unanchored scan over a 160 KB subject with `/ [ \w+ | \d+ ] \s [ x | y ] /` went from
0.28 s to 0.07 s.
