# `.deepmap` on a `Range` returns a `Range`, without calling the block

```
say (1..4).deepmap({ $_ * 2 })

raku : (2 4 6 8)
mutsu: 2..8
```

The block is never called: mutsu maps the Range's *endpoints* and hands back a
Range. The consequences go past the wrong value — a `next` inside the block has
nowhere to be handled, so

```
say (1..4).deepmap({ next if $_ %% 2; $_ })

raku : (1 3)
mutsu: Runtime error: X::ControlFlow
```

`.deepmap` on a `List` or `Array` is correct (`(1,2,3,4).deepmap(...)` answers
`(1 3)`), so this is specific to the Range dispatch, not to `deepmap_iterate`
itself — `src/runtime/methods_dispatch_match2.rs`'s `"deepmap"` arm reaches
`deepmap_iterate` for the container types, and something upstream claims the
Range first.

Found while writing `t/loop-control-without-loop.t`
(`news/2026-08/loop-control-without-a-loop.md`); the pin uses the `List` form so
it does not depend on this. Pre-existing — the same `X::ControlFlow` came out
before that work, for a different reason (the signal escaped rather than being
converted).

Worth checking `duckmap` and `nodemap` on a Range at the same time; they share
the dispatch arm.
