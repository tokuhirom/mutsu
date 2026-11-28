# An `Int` is written as decimal text without `core::fmt`, and a new `Str` hash key moves into the hash

Two costs of `%h{"key-$i"} = $v`:

- Interpolating an `Int` (and filing an `Int` hash key) went through
  `core::fmt`'s `Display`: a `Formatter`, `pad_integral` and a `write_str`
  per piece, about 350 instructions for a five-digit number. The one
  `Int`-to-decimal routine is now `str_prim::IntDecimal`, which writes the
  digits two at a time from a table into a stack buffer, as `itoa` and
  MoarVM's `MVM_coerce_i_s` do. `Joiner::push_int` (interpolation) and the
  hash lanes' `DecimalKey` both use it.
- The lean hash store copied a new key's text into a fresh `String` for the
  map, then freed the interpolation's own buffer. When the key word on the
  stack is the buffer's only holder, as a freshly interpolated key is, the
  store now moves that buffer into the map (`Value::into_owned_string`). A
  shared key (a variable, a literal) is still copied, and a present key is
  still overwritten in place without building a `String`.

callgrind, profiling build, JIT on (on top of the `for` topic-slot change):

| measurement | before | after |
|---|---:|---:|
| `benchmarks/hash-access.raku`, `BENCH_DET=1` | 76,664,973 Ir | 73,943,990 Ir |
| store loop `%h{"key-$_"} = $_ * 2`, per iteration | 3,751 Ir, 3 allocations | 3,509 Ir, 2 allocations |
