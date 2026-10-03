# Built-in methods start dispatching through a method table

ADR-11276 makes a built-in method one registered row, `(owner, name, arity,
handler)`, with invocation going through the row. Its first slice adds
`src/builtins/method_table/`. A row's `owner` is the type Rakudo declares the
method on: `elems` is `List`'s, so an `Array` finds it one MRO level up, and
`Hash` gets `Map`'s.

A call looks the row up by the receiver's `DispatchShape` and the method
symbol. `DispatchShape` is a NaN-box tag probe, now split into `List` and
`Array` and extended with `Num` and `Rat`. A miss falls back to the cascades
unchanged. The lookup map is built along each shape's MRO from the
built-in type catalog the first time it is used, not at startup.

The table replaces `builtins::fast_0arg`. That table only authorized skipping
the probes and still answered from the cascades. Its seven pairs are rows now,
on `List`, `Map` and `Str`, joined by `Num.isNaN`, `Rat.numerator` and
`Rat.denominator`, whose cascade arms now call the same handler. The lookup
runs before the two `Cool` type-object gates, which can only ever claim a type
object. In debug builds every table hit is re-answered through the full pure
path, and the two must agree; the whole `t/` suite passes with that check on.

Measured with callgrind on the profiling build, over 200,000 calls each
(second run):

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `@a.elems` | 1,440M | 1,223M | -15.1% |
| `%h.elems` | 1,434M | 1,218M | -15.1% |
| `$s.chars` | 1,078M | 956M | -11.3% |
| `$n.isNaN` | 1,325M | 780M | -41.2% |
| `$r.numerator` | 1,718M | 1,099M | -36.1% |

The late arms gain the most, as the ADR expected. A method with no row pays
only a bit test on its symbol id before the cascades.

`scripts/check-method-arms.sh` (`make check-method-arms`) is a shrinking
ratchet on the name-matching arms that remain: 487 in the pure cascades and
718 in the slow path.
