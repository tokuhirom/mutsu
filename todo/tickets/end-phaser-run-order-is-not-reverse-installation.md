# END phasers do not run in reverse installation order

Raku runs `END` blocks in reverse order of installation. mutsu runs them in
install order *within* the blocks that installed them, but always runs the
mainline's own ENDs last, so the overall sequence matches neither.

## Repro (measured 2026-08-28, release build, against `raku` as the oracle)

```raku
{ END { say "END1" } }
{ END { say "END2" } }
END { say "END3" }
```

```
mutsu: END2  END1  END3
raku : END3  END2  END1
```

`raku` is plain reverse installation. mutsu reverses the two block-scoped ones
relative to each other but defers the mainline one to the end.

## Why this needs a measurement before anyone "fixes" it

`news/2026-08/end-phasers-run-in-install-order.md` deliberately made mutsu
install-ordered to fix a different bug. Whatever that bug was, it has to keep
working: do not flip the comparator on the strength of the three-line repro
above. Establish the full ordering contract first (block-scoped vs mainline,
ENDs installed from inside a routine, ENDs installed by an `EVAL`, ENDs
installed after another END has already run) against `raku`, then change the
order once with all of those pinned.

## Provenance

Spotted while triaging
`todo/tickets/end-phaser-captured-lexical-clobbered-by-a-later-same-named-capture.md`
(a *different* END bug, about which lexicals a phaser sees rather than when it
runs). The two are independent; neither blocks the other. No roast file is known
to fail on the ordering alone — it was visible only because the
captured-lexical bug put two ENDs in the same repro.
