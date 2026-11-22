# Live `stable` and `migrate` build their stage per tap

`stable` over a live source used to register its debouncing stage when the
method ran. Over a per-tap recipe it went through the `supply_call` bridge,
which instantiated the chain below it once and shared it between taps.
`migrate` over a recipe did the same.

Both are now recipes too (mode `eager-stage`, ADR-18443). Each tap
instantiates its source for itself, builds the existing live stage over that
instance, and forwards what the stage emits. Closing the tap closes the
instance and finishes it, which also ends `stable`'s debouncing worker
(#18579).
