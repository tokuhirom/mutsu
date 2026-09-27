# Definite-return check applies only to the routine's own scope

A routine whose signature pins its return value (`--> True`, `--> Nil`, `--> 42`) may not
`return` an argument; rakudo rejects that at compile time. mutsu used to apply the check to every
`return` anywhere in the body, including those inside `if`/`for`/`while`/bare blocks. Rakudo resets
the signature info in every nested `block`/`pblock`, so a `return True` inside
`if $x { ... }` is accepted and simply returns its argument. Only a `return` in the routine's own
scope is rejected, and that includes the statement-modifier forms (`return 1 if $x`,
`return 1 for @a`), which open no block.

The two copies of the walker (one in `src/opcode.rs` for compiled routine metadata, one in
`src/runtime/registration.rs` for the metadata-less path) are now a single function. It descends
only into modifier forms, the parser's synthetic blocks, labels and a C-style `loop` header.

This unblocks loading `Usage::Utils`, whose `sub say-coloured(... --> True)` returns `True` from
inside an `if`. Pinned by `t/routines/call/return-value-spec-nested-block.t`.
