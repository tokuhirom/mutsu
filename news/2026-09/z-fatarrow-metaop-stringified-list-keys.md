# Z=> stringified a List-valued key instead of keeping it

`exec_meta_op`'s `Z` + `=>` arm built each result `Pair` by calling
`.to_string_value()` on the left operand before wrapping it in a plain `str`
`Value`. That is correct only when the left elements are already strings;
when the left side is a list of `List`-valued tuples (e.g. produced by
`cross()`), the tuple got flattened into a single space-joined string
instead of staying a `List`. `(1, 1, 1) => 0` came out as `"1 1 1" => 0`,
with the key's structure — and type — silently lost.

Per [ADR-0021](../../docs/adr/0021-argument-namedness-is-a-call-site-property.md)
I2, every data-minting site (including `Z=>`, listed explicitly in the ADR's
migration plan) must default to the positional `ValuePair` flavour and keep
the key's own value, not a stringified copy — the same rule the ordinary
`=>` operator (`OpCode::MakePair`) already followed. `Z=>`'s handler was
simply never flipped to match.

Found via the ecosystem roulette on `CellularAutomata` (locked on
[#7884](https://github.com/tokuhirom/mutsu/issues/7884)):
`cellular-automaton-from-number()` builds its rule lookup table with
`@keys Z=> @values`, where `@keys` holds 3-tuples from `cross()`. Every
`is-deeply` comparison against the expected `(1, 1, 1) => 0, ...` list
failed because mutsu's keys were joined strings instead of `List`s.

Fixed by dropping the stringification and passing the left element straight
into `Value::value_pair`, matching `MakePair`'s existing behaviour.

Pinned by `t/collections/transform/zip-fatarrow-list-key.t`.
`CellularAutomata` moves from `partial` (1/2 baseline files) to `green`
(2/2, 14/14 assertions).
