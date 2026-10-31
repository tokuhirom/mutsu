# A method name is classified once per symbol, not on every call

`shapeless_row` (the table row a receiver with no settled shape reaches, e.g. `.WHAT`,
`.HOW`, `.VAR`, `.^name`) decided what kind of name it was asked about by scanning six
string arrays (the audited `Cool` text, `Any` list, `Mu`, `Iterable`, coercion and
container lists, plus the one-argument names) on every call. That is a pure function of
the name, so it is now computed once per `Symbol` into a lock-free table
(`symbol::SymbolMemo`, the `FLAG_TABLE` mechanism for a caller's own bits) and each call
reads one atomic.

ADR-15737, measured with the profiling build against the main build taken just before
(Ir per loop iteration, slope between 100 and 300 iterations, deterministic):

| call | before | after |
| --- | ---: | ---: |
| `$x.WHAT.defined.Int` | 48,236 | 35,960 (-25%) |
| `$x.HOW.defined.Int` | 85,896 | 69,422 (-19%) |
| `$x.^name.chars` | 32,545 | 26,476 (-19%) |
| `$x.VAR.defined.Int` | 61,263 | 55,374 (-10%) |

`mooish-lazy` (`scripts/call-path-ab.sh`, slope 10 -> 50): 4,379,029 -> 3,988,269 Ir per
iteration (-8.9%; that loop spreads about +-2% run to run), allocations unchanged;
`plain-call` is unchanged (33,901 -> 33,930). These calls still cost 25-70k Ir each, so
the remaining cost is elsewhere in their path, not in the classification.
