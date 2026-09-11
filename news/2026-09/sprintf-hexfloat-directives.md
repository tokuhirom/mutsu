# `sprintf("%a", ...)` / `%A` hexadecimal-float directives

`sprintf`'s directive table had no `a`/`A` entry, so a C99 hexadecimal-float
directive fell through to the unknown-directive path and threw
`X::Str::Sprintf::Directives::Unsupported`. Both directives are implemented
now, in a new `src/runtime/sprintf_hexfloat.rs` alongside the `%e`/`%f`/`%g`
formatters.

`%a` renders a `Num` as its exact binary value: a `0x` prefix, one hex digit
before the radix point, the mantissa in hex after it, then `p` and a *decimal*
signed binary exponent — `sprintf("%a", 1e0)` is `0x1p+0`. `%A` is the same
with uppercase hex digits and `0X`/`P`.

Because an f64's fraction field is exactly 52 bits — 13 hex digits — the
rendering is exact whenever no precision is given, and trailing zeroes are
stripped. The details that are easy to get wrong, all matched against glibc:

- **Rounding is ties-to-even**, on the mantissa taken as a whole including its
  integer digit, and a carry out of that digit is **not** renormalized:
  `sprintf("%.0a", 27.1)` is `0x2p+4`, not `0x1p+5`. A carry that widens the
  mantissa keeps the requested width (`%.1a` of `1.9999999e0` is `0x2.0p+0`).
- **Subnormals** are printed with a leading `0` digit at the fixed minimum
  exponent rather than normalized, so `%a` of `5e-324` is
  `0x0.0000000000001p-1022`.
- **`#`** forces the radix point when the fraction is empty (`%#a` of `0e0` is
  `0x0.p+0`).
- **The `0` flag pads between the `0x` prefix and the mantissa** and is ignored
  when combined with `-`. The existing `apply_width` / `zero_pad_prefix_len`
  helpers already thread a sign and an `0x`/`0X` prefix out of the padding, so
  `%024a` of `-2.71` comes out as `-0x0001.5ae147ae147aep+1` with no new
  padding code.
- A `Rat` argument renders the double it denotes, like every other float
  directive, and `Inf`/`-Inf`/`NaN` keep Raku's spelling for `%A` as well —
  `%E` does not uppercase them either.

Rakudo has not implemented these directives (rakudo#6524), so there is no local
oracle; the expected values come from glibc's `printf` and from the spec test
itself.

Pinned by `t/types/string/sprintf-hexfloat.t` (38 tests) and five unit tests in
the new module. Upstream roast's new `S32-str/sprintf-a.t` — 586 subtests
covering every permutation of the flag set against widths, precisions and a
star precision — passes completely; it can join `roast-whitelist.txt` as soon
as the roast re-vendor that introduces the file has landed.
