# A `use` in an `if` body stays in the body (pinned)

A `use` inside an `if` body is invisible to a routine written outside it, while the body runs and
after it (#15698). Earlier env work already fixed it; `t/control/if-body-use-stays-in-block.t`
now pins it. The `EVAL 'use Hk; use Hk'` double hook run is pinned by
`t/lang/eval-repeated-use-runs-hook-each-time.t`.
