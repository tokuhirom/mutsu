# `Supply.stable` debounces a live supply

`Supply.stable($seconds)` on a live supply (a `Supplier`'s) returned a supply that never emitted: only a
materialized one had a best-effort "keep the last value" path. A live source is now debounced in real time: a
value is emitted once `$seconds` pass without a newer one, `done` flushes the pending value before it is passed
on, and a `quit` drops it and quits the output; the timing matches `raku` (`[2@0.4 4@1]` for the documented
burst). cro's `Cro::Tools::Services` (`Service.metadata-changed`, `.grep(...).stable(0.5)`) needs it:
`t/tools-services.rakutest` now passes 25 of 25 next to `raku`.
