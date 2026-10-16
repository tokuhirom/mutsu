# A user subclass of Supplier is a supplier

`class Tagged is Supplier { }` built an instance with none of a supplier's state, so its `emit` reached no tap,
`done` and `quit` were lost, and a `whenever` over its `Supply` never fired. The instance now carries a supplier's
state from construction (as subclasses of `Promise` and `Channel` carry their payloads), and `Supplier`'s rows answer
it like any supplier — `Supplier::Preserving` subclasses included. The second, reading-path copies of `emit`, `done`
and `quit`, which only a receiver the rows did not reach could still hit, are gone (ADR-11276 §9.133).
