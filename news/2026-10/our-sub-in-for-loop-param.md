# An `our sub` in a mainline `for -> $i` body closes over `$i`

`for 1..2 -> $i { our sub g { $i // "u" } }; say GLOBAL::g()` printed `u`;
rakudo prints `2` (#10647).

An `our sub` declared in a block of the `GLOBAL` mainline captures the block's
lexicals through the escaping-our-sub cells. The captured `my` is boxed at its
declaration, and `RegisterSub` persists the cell so a call made after the block
reads it. A single `for` parameter has neither a declaration nor a slot: the
`ForLoop` opcode binds it by name in the env, so it was never boxed or
persisted.

The compiler now records such slotless loop parameters an escaping `our sub`
captures (`CompiledCode::escaping_our_env_params`). `RegisterSub` boxes their
env binding in place and persists it, the same way the routine-nested alias
mechanism already handled them (#10512).
