# The native method row table is built with Fx hashing

Startup built the 1883-row `(owner, method)` classification table in a SipHash `HashMap` that
grew by rehashing, and de-duplicated the raw rows through a second SipHash set. Both are keyed
by static strings of our own, so they now use `FxHashMap` / `FxHashSet` sized once up front.

Measured with callgrind on the profiling build, second run (warm), same checkout before and
after:

| run | before (Ir) | after (Ir) |
| --- | ---: | ---: |
| `mutsu -e ''` | 10,106,846 | 8,385,642 (-17.0%) |
| `benchmarks/bench-grammar-parse.raku` | 30,370,168 | 28,204,671 (-7.1%) |

The grammar benchmark drifted up by about the size of the startup growth, so this brings it back
to its September level (28.2 M). Every benchmark row gets the startup saving.

Refs #10952.
