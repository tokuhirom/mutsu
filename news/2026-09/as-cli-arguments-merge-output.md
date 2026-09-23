# `run :merge` captures child output for `as-cli-arguments`

`run :merge` now captures the combined stdout/stderr stream in `.out` when no
explicit `:out(False)` disables capture, matching Rakudo, and leaves `.err`
undefined. The ecosystem sandbox also routes `XDG_CACHE_HOME` into its
writable throwaway home so precompilation warnings do not pollute captured
child output.

Found via the ecosystem roulette on `as-cli-arguments` (locked on
[#8977](https://github.com/tokuhirom/mutsu/issues/8977)). The distribution moves
from `partial` (1/2 baseline files) to `green` (2/2, 18/18 assertions).

Pinned by `t/io/proc-run-merge-captures-output.t`.
