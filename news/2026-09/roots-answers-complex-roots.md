# `Numeric.roots` answers Rakudo's complex roots, and `Complex.Str` stops inventing a second Num format

`4.roots(2)` answered `(2e0, -2e0)` where Rakudo answers
`(2+0i, -2+2.4492935982947064e-16i)`. The divergence was recorded in
[#7544](https://github.com/tokuhirom/mutsu/issues/7544)'s "plain-call
divergences" section, found by the native-method adverb sweep: the sweep can
only compare the `:qqzz9` arm against the plain arm, so a method whose *plain*
answer is already wrong shows up there without being a named-argument bug at
all.

## Two mechanisms, both in the rendering of a root

### `compute_roots` collapsed a root back to a `Num`

`src/builtins/methods_narg/numeric.rs` computed the polar form correctly and
then undid it:

```rust
if ii.abs() < 1e-12 {
    roots.push(Value::num(rr));
} else {
    roots.push(Value::complex(rr, ii));
}
```

The intent — "this root is real, so say so" — is exactly what Rakudo does not
do. `Numeric.roots` is

```raku
(^$n).map: { Complex.new-from-polar($mag ** (1/$n), ($angle + $_ * 2 * pi) / $n) }
```

so every element is a `Complex`, epsilon imaginary part and all; the
`2.4492935982947064e-16` in `4.roots(2)` *is* `2 * sin(pi)`, and Rakudo prints
it. Removing the collapse also removed the need for the zero special case:
`0.roots(2)` falls out of the general formula as `(0+0i -0+0i)`, negative zero
included, which is what Rakudo prints.

Aligning the function surfaced three more scalar-vs-list divergences in the same
routine, all now matching:

| | mutsu before | mutsu / raku now |
|---|---|---|
| `4.roots(0)`, `4.roots(-1)` | `(NaN)` — a one-element list | `NaN` — a bare `Num` |
| `4.roots(1)` | `(4)` | `4+0i` — the receiver as a `Complex` |
| `Inf.roots(2)`, `NaN.roots(2)`, `(Inf+1i).roots(2)` | `(Inf+NaN\i -Inf+Inf\i)` and friends | `NaN` |

The Inf/NaN case is a guard on the *polar magnitude*, not on the receiver's
type, which is why `Inf.roots(1)` still answers `Inf+0i`: the `$n == 1` return
happens first.

`roast/S32-num/roots.t` (whitelisted) keeps passing throughout — it writes every
case as `my @l = roots(...)`, and assigning a scalar `NaN` into an `@` variable
gives the same one-element array it was already asserting.

### `Complex.Str` had its own float formatter

The second half only became visible once the roots were `Complex`:
`say 6.123233995736766e-17` printed `6.123233995736766e-17` but
`say 6.123233995736766e-17 + 1i` printed
`0.00000000000000006123233995736766+1i`.

`format_complex` in `src/value/display.rs` carried a private `fmt_num` that
predated the `Num` arm of `to_string_value` and never learned its
scientific-notation threshold — so `Complex.new(1e16, 2)` rendered
`10000000000000000+2i` and `Complex.new(1, 1e20)` rendered `1+1e20i` (no
exponent sign). Rakudo's `Complex.Str` is literally
`$!re ~ sign ~ $!im.abs ~ 'i'`, i.e. two `Num.Str` calls, so there is nothing
for a second formatter to do.

The `Num` arm is now `pub fn format_num` and `format_complex` calls it. That is
the whole fix, and it makes the threshold unable to drift again.

## Pin

`t/roots-are-complex.t`, 26 assertions, passing under `raku` unmodified. It
covers the element type and rendering of the list form, the three scalar
returns, and the three `Complex.Str` component cases that had their own
formatter.

## Not done

`4.roots(2).^name` is `List` in mutsu and `Seq` in Rakudo. That is the return
container, not the roots, and it is untouched here.
