# A plain `Str` or `Int` piece goes straight into an interpolated string

`"key-$_"` cost about 1,840 instructions per evaluation in `benchmarks/hash-access.raku`'s insert
loop. The interpolation op (`exec_string_concat_op`) runs every piece through a dozen checks before
rendering it: a `Proxy` fetch, a container deref, a lazy `.map` Seq, a user `^find_method`, an
unhandled `Failure`, a zero-denominator Rational, `Nil`, a `Regex`, a `Buf`, a role mixin, an
`Instance`'s own `Str`, a type object, and list elements with their own stringifiers. None of these
applies to a plain `Str` or `Int`, which is what almost every interpolated piece is. An `Int` then
paid for one more `String` allocation on top, through `to_str_context` → `i64::to_string`, before
the joiner copied it.

Both shapes now go straight to the joiner. A `Str` is appended (or shared, when it is big); an
`Int` is written in decimal directly into the joiner's pending buffer by the new
`Joiner::push_int`. The one hook that can still reach a plain value, a user `^find_method`, is
honored: the fast path is off whenever any type declares one, which is a single atomic load per
op. Values that only look like a `Str` or `Int` have their own representation and keep the full
path: an allomorph (`<042>` still renders `042`), a role mixin, and a numeric-subclass instance.

Callgrind, warm, `--profile profiling`, same base:

| | before | after |
| --- | ---: | ---: |
| `benchmarks/hash-access.raku` | 144,306,977 Ir | 134,291,254 Ir (**-6.9%**) |
| `for ^10000 { @a[0] = "k-$_-$s" }` | 90,536,119 Ir | 70,024,577 Ir (-22.7%) |
| `for ^10000 { %h{"key"} = $_ * 2 }` (control, no interpolation) | 56,582,139 Ir | 56,581,091 Ir |

Pre-sizing the joiner's buffer from the pieces was also tried and measured slightly *worse*
(70.02 M → 71.07 M on the second row). Probing every piece's length up front cost more than the
single regrow it saved, so that change was dropped.

`t/lang/quoting/interpolation-plain-pieces.t` pins the rendering, including the most negative and
big `Int`s, the allomorph and the mixin. Writing it turned up an unrelated bug, now #10992: an
`Int` subclass's own `Str` is ignored by interpolation and prefix `~` (that was already the case on
`main`, not caused by this change).
