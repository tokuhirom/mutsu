# The compiled regex engine runs `&` conjunction, completing Slice B

The last part of ADR-0135's Slice B (#10252) compiles `&` and `&&`. The first branch runs inline,
in a capture level of its own. Every other branch must then match exactly the same span, which
a nested run of that branch's own compiled program checks. The run takes the first match in
priority order that ends at the required position, the same one the walk picks from its full
list of ends. All branches' captures merge as the walk merges them.

This removed a second definition of the operator. The position-only matcher behind `.comb` took
the longest end among the branches instead of requiring a common span, so
`"ab cd".comb(/ \w+ & <[a..c]>+ /)` found `ab` and `cd`, where rakudo finds `ab` and `c`. It now
asks the capture matcher (`t/regex/syntax/regex-conjunction-same-span.t`).

With this, every declarative construct of Slice B compiles: `|`, lookaround, `:i`, `:m` and `&`.
Across all of `t/` and the roast whitelist (`scripts/rx-decline-survey.sh`), compiled patterns
went from 5,954 to 5,981 and declined ones from 1,345 to 1,328. The declines that remain are almost
all later slices: `subrule` 625 (Slice D) and `code` 406 (Slice C).

Two pre-existing bugs, which both engines share, were filed on the way: `:r` does not reach
conjunction branches (#10353), and `.comb` with `:m` misses matches (#10352).
