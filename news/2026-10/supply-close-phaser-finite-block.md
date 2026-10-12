# A finite `supply { }` block runs its CLOSE phaser when it ends

A `supply { }` block with no live `whenever` ends with its body. Its CLOSE phaser used
to run only if the tap was later closed explicitly, so
`supply { CLOSE say "CLOSE"; emit 1 }.tap` never printed `CLOSE`.

`Supply.tap` now records how such a block ended and runs its close callbacks once, through
the new `Interpreter::run_supply_close_callbacks`, which closes the block's nested
`whenever` taps first. The order matches rakudo: after the downstream `done` for a plain
end of the body, and before the `done` or `quit` for a body `done`, a `die`, or finished
cold `whenever`s. A `react` `whenever` over such a block runs it too, once the body has
opened no live subscription. Taking the callbacks keeps the run-once guarantee against a
later `Tap.close`. (#12873)
