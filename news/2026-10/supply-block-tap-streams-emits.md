# A tapped `supply { }` block streams its emits to the tap

A plain `.tap` of a `supply { }` block used to run the whole block first and
then replay everything it had emitted, so
`supply { emit 'a'; sleep 0.3; emit 'b' }` delivered `a` 0.3 s late, together
with `b` (#11434). Terminal::MultiProgress's odometer test drew only one
running frame because of this, and that frame looked as if the odometer had
gone backwards.

The tap now puts a small stream record on the block's emit-buffer frame, and an
`emit` on the block's own emitter hands the value to the tap right away. The
`.do` callbacks, delay and throttle run there too. The `whenever`
subscriptions the block registers are still buffered and wired up once the
block has returned. That is also Rakudo's order:
`supply { emit 0; whenever $cold { emit $_ }; emit 3 }` now delivers `0, 3`
before the `whenever`'s values, where mutsu used to slot them in between.

A tap callback that dies stops the block at that `emit` and throws out of
`.tap` without calling `quit`, the same as in Rakudo.
