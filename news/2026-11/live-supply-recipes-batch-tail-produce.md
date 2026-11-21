# Live `batch`, `tail`, `produce`, `elems` and `words` are per-tap recipes

ADR-18443 slice 2b. Over a plain live source (or a recipe already built on
one), `Supply.batch`, `.tail`, `.produce`, `.elems` and `.words` no longer
register a stage on the source when the method runs. They return a recipe
that each `tap` instantiates with its own state: its own pending batch, last
values, accumulator, count or partial word. An untapped stage does no work,
two taps run two chains (as in rakudo), and closing a tap or completing a
`head` above the stage releases the source.

The per-tap state lives in `native_methods/supply_derive_buffer.rs`, and the
word splitting, batch period and `elems` reporting there are now also what
the eager supplier stages use, so the two paths share one implementation.

A `react` `whenever` over a recipe, and a combinator that is not a recipe
yet (`zip`, `classify`, ...), now instantiate it through the recipe's own
`tap` (`instantiate_live_recipe`) into a supplier of their own, instead of
rebuilding the old eager stages; that rebuild only knew `map`/`grep`/`do`/
`head` and silently turned every other stage into a `grep` (a `react` over
`.produce(...)` saw nothing). The react closes its instance when the
`whenever` or the react ends, which releases every stage below it.
