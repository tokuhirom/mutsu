# `whenever` pointy blocks take a full signature

`whenever $supply -> ... { }` used a hand-rolled parser that understood only a
single, optionally typed, parameter. Anything else — most visibly a
sub-signature like Temp::Path's `whenever $GOODS -> ($_, $path?) { ... }` —
failed to parse, so the statement fragmented into a bare `whenever` word plus a
standalone pointy block. At the top of a module that surfaced as an
`Undeclared routine: whenever` compile error; loading `Temp::Path` hung at
`END` instead, because its `react` never subscribed and the `await
$GOODS.closed` in its END phaser waited forever. That hang is why
`BuildToolchainRepo::Lang::CProbeHelpers`, which `use`s Temp::Path, never
finished loading.

`whenever` now parses its pointy block with the ordinary pointy-block parser,
so it accepts every signature a `.tap(-> ... { })` block does: sub-signatures,
optional and `where`-constrained parameters, array destructuring. The AST node
carries `params`/`param_defs`, and the compiler pools the callback as an
anonymous `SubDecl`, so the VM reads the signature back through the shared
`closure_signature` cache (the `WheneverScope` opcode lost its two
constant-pool name/type operands).

Binding `$_` through a sub-signature also works now: the callback dispatch no
longer resets or overwrites a topic the signature itself binds, so
`-> ($_, $path?) { when 'add' { ... } }` tests the command, not the whole
emitted list.

Pinned by `t/concurrency/supply/whenever-pointy-signature.t`.

Closes #9492.
