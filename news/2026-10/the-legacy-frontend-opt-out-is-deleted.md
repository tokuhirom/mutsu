# The `MUTSU_LEGACY=1` frontend opt-out is deleted

Stage 3 of ADR-10723 kept `MUTSU_LEGACY=1` (and `MUTSU_RAKUAST=0`) as an opt-out to the parser's own
tree. It is gone, with the `all` level, the `Mode` enum, `scripts/rakuast-frontend.sh legacy` and the
CI step that ran it: the main program and every `EVAL` string always run as `parse -> RakuAST ->
lower`. `use`d modules still compile from the parser's tree until #15074 is fixed. Four
module round-trip gaps found while measuring that are fixed: a `%(…)` argument of a role
application, the `$<a=b>` back-reference spelling, a statement modifier inside a heredoc's `{…}`, and
`also is Base` in a grammar body.

The post-merge CI step that exercised it (#15060) goes with it.
