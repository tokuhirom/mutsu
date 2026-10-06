# RakuAST: `rakuast-frontend.sh causes`

The refusal survey that picks each RakuAST slice lived in an untracked script.
`scripts/rakuast-frontend.sh causes` is that survey: it runs every `t/` file
outside the ratchet under `MUTSU_RAKUAST=1`, records `PASS`, `DIFF` (runs, but
fails differently from the ordinary frontend) or `REFUSE` (the first construct
the conversion or the lowering refuses) in `tmp/rakuast-causes/results.tsv`, and
`scripts/rakuast-causes.py` prints the causes by file count.

A file is counted under its first refusal only, so a count is an upper bound on
what fixing that cause moves into the list. On 2026-10-06 it reported 2409 files
outside the list: 2211 refusals and 198 files that run but behave differently.
