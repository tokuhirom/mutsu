# RakuAST is the default frontend (ADR-10723 Stage 3)

Every program unit, the main program and each `EVAL` string, now runs as
`parse -> RakuAST -> lower -> compile` by default. This is the path that
`MUTSU_RAKUAST=1` selected until now. `MUTSU_LEGACY=1` opts out to the parser's
own tree for one release; `MUTSU_RAKUAST=0` is accepted as the same opt-out, and
`MUTSU_RAKUAST=all` / `=convert` still select the wider and the passthrough-free
levels. `use`d modules keep going parse -> compile in the default mode.

The precompilation cache key now carries the frontend level, so caches written
under different levels are never mixed. `--dump-ast` prints the lowered tree by
default, as it did under `MUTSU_RAKUAST=1`.

`scripts/rakuast-frontend.sh check` now runs `ci/rakuast-frontend-passing.txt`
under `MUTSU_RAKUAST=convert` (the default already runs every `t/` file through
`make test`), and `scripts/rakuast-frontend.sh legacy` runs all of `t/` under
`MUTSU_LEGACY=1` so the opt-out cannot rot.
