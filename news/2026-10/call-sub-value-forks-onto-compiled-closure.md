# `call_sub_value` runs compiled closures as bytecode

ADR-0055 slice 4. A closure carrying `compiled_code` that a native routine calls
back (`Proxy` FETCH/STORE, `Promise.then`, `cue`, Supply taps, comparators, ...)
now goes through `call_compiled_closure` like every other closure call, instead
of the tree-walk carrier with its own env merge and writeback. The gate is the
closure instance, never the call site. Two shapes keep the carrier for now:
`supply` block bodies (they get their emitter and the panic-to-QUIT boundary
there) and closures writing a free variable they hold no shared cell for -- a
phaser such as `CLOSE { $done = True }` built at block entry, before the
`my $done` it names has run (TODO: hoist a phaser's block-declared lexicals into
cells, then drop that condition).
