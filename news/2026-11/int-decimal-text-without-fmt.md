# An interpolated `Int` is written without `core::fmt`

`"key-$i"` wrote the `Int` piece with `write!(pending, "{i}")`. That goes
through `core::fmt::write`, `Display::fmt` and `Formatter::pad_integral`:
about 215 instructions for a five-digit number. The VM's `Int` hash-key text
(`DecimalKey`, #19289) went through the same `write!`, into a stack buffer.

`DecimalKey` now writes the digits itself, two at a time from a `"00".."99"`
table, into its 20-byte stack buffer. It moved to the value layer
(`value::decimal_text`) and is re-exported as a `Str` primitive. The
interpolation joiner's `push_int` and every VM `Int` hash key share that one
routine. A unit test pins it against `i64`'s `Display` at the edges.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-hash.raku` | 177,662,612 Ir | 173,592,977 Ir (-2.3%) |
| `benchmarks/hash-access.raku` | 76,699,594 Ir | 75,141,752 Ir (-2.0%) |
| 20,000 × `my $s = "key-$i"` | 126,178,364 Ir | 123,129,663 Ir (-2.4%) |

`Value::to_string_value` keeps `i64::to_string`, which the standard library
already specializes without `core::fmt`; routing it here measured slower.
