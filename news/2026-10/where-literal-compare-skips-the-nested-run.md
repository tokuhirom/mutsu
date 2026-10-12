# A `where * < N` predicate no longer runs a nested VM per check

A `where` clause or a `subset` predicate that is a single comparison of the
value against an integer literal (`where * < 10_000`,
`subset Small of Int where * < 100`) used to run its precompiled body on every
check. Each run was a nested VM entry with a register save and restore and a
`$_` bind and unbind, about 2.4k instructions for one integer comparison.

The shape is now recognized once: when a parameter's `where` chunk is compiled
(`ParamChunks::where_int_compare`), and when a subset is registered
(`SubsetDef::predicate_int_compare`). A check on a plain Int value calls the
same comparison routine the `NumLt`/`NumLe`/`NumGt`/`NumGe`/`NumEq` opcodes
use (`num_lt_values` and the others). The check falls back to the body when
the value is anything else (a BigInt, a Rat, a Str) or when any user candidate
of the operator exists, so a user `infix:<<>` still decides the predicate.

On the subset/where section of `bench-multi-dispatch` (30,000 calls, each
checking `Small` and usually also `where * < 10_000`), callgrind went from
805.6M to 623.7M instructions (-22.6%). This is part of #10111.
