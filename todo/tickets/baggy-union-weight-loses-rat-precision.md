# Baggy union adds weights as `Num`, so a `Rat` weight loses precision

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/operators.rakudoc:1795`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
say <a b c> (+) (a => 2.5, b => 3.14).Mix;
# raku:  Mix(a(3.5) b(4.14) c)
# mutsu: Mix(a(3.5) b(4.140000000000001) c)
```

## Narrowed

The `Rat` survives everywhere except the union's own addition:

| Program | raku | mutsu |
|---|---|---|
| `say (3.14 + 1).raku` | `4.14` | `4.14` |
| `say (2.5 + 3.14).raku` | `5.64` | `5.64` |
| `say (a => 3.14).Mix.raku` | `("a"=>3.14).Mix` | same |
| `say <b> (+) (b => 3.14).Mix` | `Mix(b(4.14))` | `Mix(b(4.140000000000001))` |

So the `Mix` stores `3.14` as a `Rat` and ordinary `Rat + Int` is exact; it is
`(+)` (`infix:<(+)>`, baggy addition) that coerces the two weights to `f64`
before adding. `a(3.5)` happens to be exact in binary, which is why only the
`b` entry shows it.

## Where to look

`src/vm/vm_set_ops.rs` and the Bag/Mix weight arithmetic in `src/builtins/`.
The fix is to add weights with the same numeric-tower promotion ordinary `+`
uses (`Int`+`Rat` stays `Rat`, only a real `Num` operand makes the result
`Num`), rather than going through `f64`.

## Neighbourhood to check when fixing

`(-)`, `(.)`, `(^)` and the `⊎`/`∩`/`∖` spellings; `Bag`/`BagHash`/`MixHash`
receivers; a `FatRat` weight; and `.total`/`.roll` weight sums, which may share
the same coercion.
