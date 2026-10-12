# Closing a supply block's tap closes the `whenever`s opened inside its bodies

A `whenever` written inside another `whenever`'s body subscribes long after the
`supply { }` block was tapped, from whichever thread delivered the value. Its
subscription used to be recorded nowhere, so closing the block's tap (or the
block finishing with `done`) never reached it and its own `CLOSE` phasers never
ran (#12864).

Each block now keeps the set of subscriptions its bodies opened, keyed by its
emitter, as Rakudo's `SupplyBlockState.active` does. Closing or completing the
block closes every live one before the block's own `CLOSE` phasers run; a
subscription whose source finished leaves the set on its own, so a long-lived
block does not accumulate dead taps.

This is what lets `Cro::HTTP::Server.stop` close the connections it accepted:
`Cro::TCP::Replier` closes the socket only in such a nested block's `CLOSE`.
