# `nqp::elems` and `nqp::atpos_i` answered from a whole-vector copy

Encoding one long string with the vendored `JSON::Fast` was quadratic in that
string's length. A 8,000-character string cost 0.305s per `to-json`; 4,000
cost 0.089s and 2,000 cost 0.029s — doubling the input multiplied the time by
2.3, then 2.7, then 3.1, then 3.4, climbing towards 4. It is linear now, and
the 8,000-character encode costs 0.047s: **6.5x faster**, with the margin
widening as strings get longer.

The cause was not in the module. `JSON::Fast`'s `str-escape` walks a string's
NFD codepoints with nqp's idiomatic scan,

```raku
my $codes := text.NFD;
nqp::while(
  nqp::islt_i(++$i, nqp::elems($codes)),
  ... nqp::atpos_i($codes, $i) ...
);
```

and both of those ops reached their target's elements through
`Interpreter::nqp_elems_of`, which answers with an owned `Vec<Value>` —
`items.to_vec()` for an array-backed value, `with_buf_elems(|e| e.to_vec())`
for a buf-backed one. So each of the n iterations copied all n elements,
twice: once to compare `$i` against a length, and once to read a single
codepoint. `nqp::elems` is the more expensive half, because it is a loop
*condition* and nqp code re-evaluates it every pass.

Measured on its own, with the bare loop subtracted, `nqp::elems` over a `Uni`
grew by 3.5x, then 3.8x, then 3.9x per doubling — textbook O(n^2).

The O(1) primitive already existed. `value_buf::buf_elem_at` was written for
exactly this reason, its doc saying it "exists so a hot `@words[$i]` loop does
not pay `decode_elems`'s whole-buffer `Vec<Value>` per access", and
`value_buf::buf_len` answers a length by dividing the node's byte count by its
element width rather than decoding anything. The Raku-level subscript had been
routed to them; the `nqp::` layer never was. `nqp_elems_len_of` and
`nqp_elem_at` now bridge that gap — an array-backed value answers from
`items.len()` / `items.get(idx)`, a buf-backed one from `buf_len` /
`buf_elem_at` — and `nqp::elems` and `nqp::atpos_i`/`atpos_n` use them.
`nqp_elems_of` stays for the callers that genuinely want every element
(`nqp::splice`, `nqp::strfromcodes`, the `'$!reified'` storage bind).

`t/collections/nqp-uni-element-access-is-linear.t` pins it. Its complexity guard is a
*ratio* between two scans in one process, so machine speed and CI load cancel
out instead of setting the threshold: a 4x longer input costs ~4x linear and
would cost ~16x quadratic, and the test asserts the ratio stays under 9. It
fails on the pre-fix binary and passes after, which is how the bound was
chosen rather than guessed.

## What this does not fix

This is one of the two things [#8289](https://github.com/tokuhirom/mutsu/issues/8289)
measured, and it is not the larger one. That issue's headline figures —
~63x rakudo on encode and ~126x on decode for a 2,380-byte META6-shaped
document — barely move here (encode 1.068s -> 1.049s over 50 iterations),
because that document is made of many *short* strings, where the difference
between n and n^2 is nothing. The quadratic is a real bug with a real fix, but
it is a separate bug that happened to live in the same benchmark.

What remains is flat and structural, and a profile says so: mutsu's plain
`+` is 2.3x *faster* than rakudo's, while a named sub call is 4.1x slower and
an `nqp::` op about 3x slower. The cost is in calls, not in the VM loop. A
callgrind run over three decodes (1.2 billion instructions) puts
`alloc::fmt::format::format_inner` at 8.3% inclusive across 113,055 calls,
every top caller of it being dispatch-key construction —
`find_compiled_function_inner`, `candidate_search_packages`, `has_proto`,
`resolve_proto_function`. Add `__memcmp_avx2_movbe` at 3.8%,
`function_key_base_name` at 2.1%, and the malloc/free family at 15.5% (about
347,000 deallocations per decode), and the shape is clear: mutsu resolves
function calls by building and comparing strings. Fixing that is a dispatch
change, not a JSON change, and wants its own issue.

A third finding fell out of the same measurements and belongs to neither:
invoking a `Callable` value costs 47-62x rakudo (`$b(...)` 0.2624s against
0.0056s over 100,000 calls), and 4.2x more than calling a *named* sub in
mutsu itself — where rakudo has it the other way round, a closure call being
the cheaper one for want of multi-dispatch. `JSON::Fast`'s decode path makes
only 163 such calls, so it is not implicated here, but the ratio is the worst
of any measured.

All numbers above are local `now`-delta and `callgrind` measurements, taken to
scope the work. Per the repository's convention they are not authoritative for
`PERFORMANCE.md`; a JSON benchmark under `benchmarks/`, tracked by the bench
CI, would be the way to hold these honest over time.
