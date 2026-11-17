# A Bool stored into `my int` takes the native fast type check

`my int $hit = $a < $b` -- a native flag computed from a comparison, the shape
of every per-iteration flag in a native loop -- missed the declaration's native
fast check, which only accepted a value already tagged `Int`. The Bool then went
through the general type-check path (constraint normalization, alias probes,
the full matcher and its coercion) only to be unboxed to `0`/`1`, which is what
a `Bool` (an `Int` enum) becomes in a native `int`.

The fast check now does that unboxing in place for `int`/`int64` under the same
gates as before (no subsets, plain-builtin constraint). A `num` still rejects a
`Bool` through the general path, and a boxed `my Int $x = True` keeps the Bool.

Callgrind, release build: a 1.1M-iteration loop declaring
`my int $matches = <comparison chain>` per iteration
(`Red::Regex::Unicode::property-ranges('Lu')`) went from 16.12G to 14.69G
instructions (-8.9%); a standalone copy of the loop from 9.0k to 8.0k
instructions per iteration. Refs #15498.
