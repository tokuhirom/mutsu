# nqp::objprimspec, coerce_is/coerce_si and unbox_s

Swept `ecosystem/`'s `blocked_load` records for `nqp::`-related "Unsupported
nqp:: op" load failures and implemented the four that were bounded value
transforms:

- `nqp::objprimspec($type)`: the REPR primitive-storage code for a type
  object (0 boxed, 1 signed-int family, 10 unsigned-int family, 2 num,
  3 str — the split between 1 and 10 verified against
  `raku -e 'nqp::objprimspec(uint32)'`). `AttrX::Mooish`'s `is mooish` trait
  handler uses it to reject attributes declared with a native type, and is a
  hard dependency of `Async::Workers` and `Cooklang`.
- `nqp::coerce_is` / `nqp::coerce_si`: native int <-> str coercions.
  `Net::Netmask::Fast` uses both to stringify netmask bit counts and parse
  CIDR/octet strings (`coerce_si` follows `strtol` semantics: skip leading
  whitespace and an optional sign, stop at the first non-digit, saturate
  rather than error on an out-of-`i64`-range magnitude).
- `nqp::unbox_s`: the native str inside a boxed `Str` — mutsu has no
  separate native-str representation, so it is just the value's string
  form. `Net::Netmask::Fast` unboxes its `Str:D` constructor parameters
  this way.

Net::Netmask::Fast's `use` now succeeds (`blocked_load` -> `partial`, 2/4
baseline files matching rakudo exactly); the other two hit unrelated bugs
(a mask-bits parsing issue and a missing `Int` method) not addressed here.
Async::Workers and Cooklang's load now gets past the `nqp::objprimspec`
abort into a different, unrelated failure — role-composed private-method
dispatch through `AttrX::Mooish::X::Wrapper` — filed as
[#8573](https://github.com/tokuhirom/mutsu/issues/8573).

`nqp::getlexdyn`, needed by `Rakudo::Options` for `%*COMPILING` (a
compiler-internal dynamic lexical mutsu has no equivalent for), turned out
to be architectural rather than a value transform and was filed instead of
stubbed: [#8572](https://github.com/tokuhirom/mutsu/issues/8572).

Regression test: `t/types/coercion/nqp-objprimspec-and-str-int-coerce-ops.t`.
