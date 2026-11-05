# The `MUTSU_RAKUAST=convert` test mode is retired

RakuAST C1 step 2 (#15759). `MUTSU_RAKUAST=convert` ran every unit through the spelling-keeping parse, `convert` and `lower`, and
`ci/rakuast-frontend-passing.txt` (8722 files) pinned what passed under it. Its coverage now comes from two ratchets over the public
API: `EVAL(slurp($f).AST)` over the files of `ci/rakuast-ast-eval-passing.txt` (8996 files, the write direction, cold precompile
cache like CI), and `scripts/ast-text-corpus.sh check` (the `.AST` text against rakudo's, the read direction).

Gone: `convert_level`, the `ast-convert` cache tag, `lower_converted`, the `check`/`survey`/`update`/`causes` modes of
`scripts/rakuast-frontend.sh`, `scripts/rakuast-causes.py`, the list and its CI step, and `t/rakuast/rakuast-frontend-mode.t`
(which pinned the variable). The default frontend is unchanged and is what `prove t/` runs.

Known gap, recorded on #15759: about 235 files that passed under the convert mode do not yet pass as `EVAL($ast)` (execution
differences of an EVAL'd unit, not conversion refusals), and about 78 more read `$?FILE` and can never move over.
