# A grammar action's `make` call skips the call funnel after its first run

Every grammar action method ends in `make ...`, so a YAMLish parse makes about
a thousand `make` calls. The per-site route memo for built-in routines (#18028)
lets a call such as `abs($x)` skip the general `CallFunc` funnel once its site
has been answered by the builtin. It never remembered `make`, which has a lane
of its own (`try_make_builtin_call`) placed after most of that funnel. Every
`make` call therefore paid for the resolution caches, the argument
normalization, and the shadowing probes before it reached its lane.

A `make` call that its lane answered now remembers that route for its site
(`ROUTE_MAKE`), under the same registry write generation the native routes
use. Each later call re-checks what can change while the program runs: a
lexical or env `&make`, a wrap, an EXPORT-hook override, a `my method`, or
the argument shape. Then it runs the lane directly. The lane also reads the
frame's `$/` once instead of twice: once for the "must be a Match" check and
once for the `.made` write.

A minimal action (`method w($/) { make ~$/ }`), `MUTSU_JIT=off`, callgrind:
**-2.9k instructions and 3 allocations per `make` call**. One warm
`load-yaml` of `bench-yaml-parse-big`'s document: **134.9M -> 131.2M
instructions (-2.8%)**, 76.0k -> 72.2k allocations (#12917).
