# Live `lines`, `reduce`, `unique` and `squish` are per-tap recipes

The second slice of ADR-18443 (#18443): over a plain live source, or over
another per-tap stage, `Supply.lines`, `.reduce`, `.unique` and `.squish` no
longer register a stage on the source when the method is called. Each tap
builds its own stage when it subscribes -- with its own line buffer,
accumulator or seen-set -- and closing the tap releases the source. An
untapped `$live.unique(:as(&key))` therefore never calls `&key`, and two taps
of one `$live.unique` each see every first occurrence, as in rakudo.

`unique(:expires)` and the `Supplier::Preserving`, `schedule-on` and
seeded-value sources keep their eager stages for a later slice, as do
`batch`, `tail`, `produce`, `elems`, `words`, `zip` and `classify`.
