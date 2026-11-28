# The Cool-only method gate answers each name once

`is_cool_only_method` decides whether a method name is one `Any` and `Mu` cannot answer
(ADR-0051 P5). The native method dispatch asks it on every method call it serves: `.key`,
`.value` and `.Str` in JSON::Fast's encoder, for example. The answer depends only on the
name, but each ask probed the method catalog for `Cool`, `Any` and `Mu` again, comparing strings
in the snapshot tables. That was ~700 instructions per call.

Each name is now decided once per thread and memoized. The memo is capped, so a program that
generates unboundedly many method names is answered without caching past the cap.

`bench-json-fast-encode` (`scripts/bench-det.sh`, `+jit`, local A/B): 846.4M -> 827.6M
instructions (-2.2%).
