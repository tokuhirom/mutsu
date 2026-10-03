# check-name-scans keeps a frozen per-file baseline

`make check-name-scans` counts run-time package-name string surgery
(`format!("{pkg}::{name}")`, `== "GLOBAL"`, `"::"` splitting) outside the
parser and compiler. Its baseline used to be three totals that every PR
lowering a count had to re-cut, so parallel PRs shrinking different files
conflicted on the same three lines. That blocked the campaign in #11507, which
aims to bring all 577 sites to zero.

The baseline is now one row per file (`<path> <qualify> <global-cmp> <scan>`),
and the check fails only when one counter of one file rises above its row. A
file with no row is allowed zero. A drop needs no re-cut, so a PR that removes
sites edits no shared file. Comparing per file also closes a hole that totals
had: a drop in one file can no longer pay for a new site in another.
`--update` still tightens every row at once, but it is now optional, and
`--self-test` covers the per-file comparison as well as the patterns.
