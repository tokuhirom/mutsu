# `@a[$i] = $v` into a declared local skips the name-keyed store preamble

An element store into an `@` local the running code declares compiles to `IndexAssignLocal`, and
ADR-14632 already made that slot the authoritative place of the container. But the store was
reached through a preamble that asked, by name, every question the general store needs: a bind
expression, an attribute mirror, a package-body seed, a user `postcircumfix` remap, and the older
fast lane's probes (`unit_lexical_container_cell`, `unit_lexical_slot`, the `our` key, an env read
of the binding, the slot's constraint record). About 2,500 instructions per store.

A lean lane now runs first and answers those questions from where they are settled: the chunk
(a plain user `@` lexical declared here, memoized per slot), the process latches the place rule
uses, the container's embedded metadata, and the value and index on the stack. A `my @b := @a`
alias record is followed only when it names a variable bound to the same node. Anything else falls
through to the unchanged general store.

`SinkPopAssign`, an assignment statement's sink, is now admitted by the JIT like `SinkPop`. A loop
body holding an element store used to be rejected from the JIT whole.

Measured with callgrind on 100,000-iteration loops (#19285): the marginal cost of a plain
`@arr[$i] = $v` fell from ~2,660 to ~460 instructions, and a store through a `:=`-bound array from
~2,970 to ~1,100. A tenth-size `bench-index-store` runs 21% fewer instructions.
