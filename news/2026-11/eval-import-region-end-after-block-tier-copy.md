# An EVAL unit ends cleanly after a block binds a lexical stub in its import layer

`try { require Foo::Bar }` inside an EVAL unit that holds a `use` bound the
`require` stub in the unit's env import layer while the `try` block's own tier
sat above it. The write copied the tiers in between, the block's exit then took
the whole env back (the tier fallback), and ending the unit found the layer
under an extra tier: a debug-build assertion failure (`EVAL(slurp($f).AST)` of
`t/exceptions/begin-phaser-catch-handler.t` and `t/vm/scope/lexical-type-scope-exit.t`).
Ending the region now folds such a chain into one flat tier, the same end as for
a flattened env. The `eval-check` ratchet runs without a debug-build panic.
