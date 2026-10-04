# Numeric Int, Num and Bool coercions are method-table rows

`Int`, `Num` and `Bool` called with no arguments on `Int`, `Num`, `Rat` and
`FatRat` are now rows in the built-in method table (ADR-11276). `Complex` gets
only a `Bool` row: its `Int` and `Num` read `$*TOLERANCE`, which needs the
interpreter. Each coercion has a single implementation, used by the rows, by
the remaining cascade arms and by `Str.Int`'s parse-then-truncate path. The
per-type copies that had built up before are gone.

`1e30.Int` and `"1e30".Int` now give `1000000000000000019884624838656`, as
Rakudo does. Before, a `Num` past a machine word saturated at
`9223372036854775807`.

The table can now refuse more calls by bit tests alone. Besides checking the
argument count, it keeps a mask per method name of which receiver types have a
row. A call like `"42".Int` therefore skips the lookup outright: `Int` has rows
on the numeric types but none on `Str`.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `$r.Num` (Rat) | 1,837M | 338M | -81.6% |
| `$r.Int` (Rat) | 1,828M | 407M | -77.7% |
| `$i.Int` | 1,004M | 291M | -71.0% |
| `$i.Num` | 1,014M | 296M | -70.8% |
| `$n.Int` (Num) | 1,014M | 308M | -69.6% |
| `$i.Bool` | 1,365M | 843M | -38.2% |
| `"42".Int` (no row) | 1,638M | 1,659M | +1.3% |
