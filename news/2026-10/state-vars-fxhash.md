# `state` variable store hashes with FxHash

The interpreter's `state_vars` map, read and written on every call of a sub that
declares a `state` variable, used the SipHash default hasher while its sibling
lexical-state maps already used `FxHashMap`. Its key is a `(Symbol, Option<u64>)`
pair of small integers, so SipHash bought nothing. Switching it to `FxHashMap`
cuts a debug build's cost of the `sub foo () {$ = 42}; for ^N { $ = foo }` loop
from about 258k to about 231k instructions per iteration (callgrind), which is the
loop that dominates `roast/S04-declarations/state.t` (about 159 s on a debug
build, about 11 s on release).
