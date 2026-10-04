# keys, Numeric and Int on lists and maps are method-table rows

The built-in method table (ADR-11276) now has rows for `keys`, `Numeric` and
`Int` on `List` and on `Map`, and for `chars` on `Cool`. An `Array` reaches
the `List` rows through its MRO, and a `Hash` reaches the `Map` ones.

`List.keys` is the same lazy counting Seq as before. `Map.keys` yields an
object hash's real key objects and a plain hash's decoded string keys. The
general `.keys` arm calls these same handlers. A list or map numifies to its
element count. `Cool.chars` stringifies its receiver first: `12345.chars` is
5, and `[1, 2, 3].chars` counts the characters of `"1 2 3"`. Because `Str` and
`Cool` share one `chars` handler, every receiver type uses the same code.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `@a.Int` | 1,969M | 396M | -79.9% |
| `%h.Numeric` | 1,910M | 395M | -79.3% |
| `$i.chars` | 1,077M | 378M | -64.9% |
| `@a.keys` | 2,701M | 1,024M | -62.1% |
| `%h.keys` | 3,244M | 1,296M | -60.0% |
| `@a.sum` (no row) | 2,784M | 2,778M | -0.2% |
