# Cheaper cell drops and grep predicates

Two costs that every inline `.grep` paid per element are smaller now (#19323):

- **Dropping a shared element cell.** A `ContainerCell` that still has other
  handles is checked for acyclicity on every drop, so the cycle collector can
  skip it. That check took three locks: the value slot, the QuantHash weight
  and the `is default` value. Almost no cell ever has a weight or a default.
  A sticky `side_edges` flag, set before either is first given a value, now
  lets the check skip those two locks.
- **Grep predicates.** A predicate that is a plain `Bool`, `Int`, `Num` or
  `Str` is answered by `Value::truthy` directly. Before, it went through the
  Seq pull, the regex check and the user-`Bool` dispatch probes, all of which
  answer the same thing for such a value.

The grep loop also stopped writing an unobservable topic back into its source
item.

`benchmarks/array-ops.raku` (callgrind, JIT on, second run) goes from
398,235,306 to 357,012,328 instructions (-10.4%). That is about 2,680
instructions per callback invocation.
