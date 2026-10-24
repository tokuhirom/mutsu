# `state` declarations re-entered in a loop stop rebuilding their bookkeeping

`StateVarInit` runs on every entry to a `state` declaration, including the entries after the first
that only re-bind the stored cell. That steady state formatted the state key into a `String`
(`name#r12`, ...) to compare it with the env metadata entry, hashed the variable name for each of
five env probes, copied it into an owned `String`, and re-derived the variable's type-constraint
lookup by name. It now probes by the slot's interned symbol, compares the metadata text without
building it, and skips the type lookup when no typed lexical exists in the program.

`roast/S04-declarations/state.t` spends ~13 s of its ~13.5 s in `for ^2000000 { $ = foo }` with
`sub foo () {$ = 42}` (#14788). That loop, callgrind with the profiling build, warm second run,
difference of a 10,000 and a 5,000 iteration run: 24,454 -> 19,962 instructions per iteration
(-18.4%). The file is still ~4x rakudo's time; the rest is the call itself (~7.5k) and the cell
store in `SetLocal` (~1.7k each).
