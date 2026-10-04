# List.join and Str are method-table rows

Two more stringification methods are now rows in the built-in method table
(ADR-11276): `List.join`, with or without a separator, and `.Str` on `Str`,
`Int`, `Num`, `Rat`, `FatRat` and `Complex`.

Both rows hand anything that needs the interpreter back to the general path:

- `join` declines a list that holds an object whose `.Str` might be user
  code, a Junction, a deferred `.map` Seq, a `Proxy`, or an undefined element.
  Rakudo warns for each undefined element; issue #11838 tracks that warning.
- `.Str` declines a rational with a zero denominator, whose error needs the
  interpreter's context.

The zero-argument and one-argument `join` arms had drifted apart. Only the
one-argument arm resolved holes in an `is default` array and refused mixins
and lazy elements. Both now share a single implementation, which the row also
uses.

One answer changes, towards Rakudo: `(a => 1).join("=")` is now `a\t1`, the
Pair's own `.Str`. Joining a Pair joins the one-element list holding it, so
the separator never appears. Before, mutsu returned `a=1`.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `$s.Str` | 1,311M | 359M | -72.6% |
| `$i.Str` | 1,131M | 416M | -63.2% |
| `$n.Str` | 1,293M | 573M | -55.7% |
| `$r.Str` | 2,823M | 1,304M | -53.8% |
| `$l.join("-")` | 2,645M | 1,247M | -52.8% |
| `@a.join` | 2,983M | 1,441M | -51.7% |
| `@a.join(",")` | 3,040M | 1,538M | -49.4% |
| `%h.Str` (no row) | 2,928M | 2,951M | +0.8% |
