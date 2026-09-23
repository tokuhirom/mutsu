# TRIR runs `nqp::elems` / `shift_i` / `push_i` without the dispatch table

ADR-0112 Step 3's first slice. JSON::Fast's escaped-string path
(`unjsonify-string`) is a loop of three list ops per character:
`nqp::elems(codes)`, `nqp::shift_i(codes)` and `nqp::push_i($output, ...)`.
In TRIR each of them was a generic `NqpOpGen`, which builds an argument
vector, walks the op table by name and boxes the result. `shift_i` also
converted its boxed result by re-reading the op's own name
(`coerce_like`'s `rsplit_once('_')`), about 240 instructions a character on
its own.

They now have typed TRIR ops, `ElemsO`, `ShiftIO` and `PushIO`. Each calls
the body the dispatch table runs, so the two cannot drift: `nqp_elems_count`,
`nqp_shift_int` and `push_elem`. `elems` and `shift_i` answer straight on the
native bank, so `my uint32 $o = nqp::shift_i(codes)` is a native store that
wraps. `elems` of a hash, or of anything else that is not list-ish, goes to
the generic op, and so does `push_i` of a value that is not a native int.

Measured on a release build in a 4-core container:

- slow-string loop, 400-character strings: 546 → 359 ns/char;
- the 727-record SPDX `from-json`: ~0.21 s → ~0.17 s (rakudo: 0.044-0.061 s).

Profiling for this slice found two costs that are too big for it, now filed
under #8673. `nqp::shift` is `Vec::remove(0)`, so consuming a list from the
front is quadratic: 2,247 ns/char at 2,000-character strings (#9121). An empty
`[]` element costs ~138K instructions, mostly the untyped bareword resolution
of `List` / `Array` / `IterationBuffer` and the attribute ops (#9122).

Pin: `t/vm/codegen/adr0112-trir-list-ops.t` checks that TRIR on, TRIR off and
rakudo's transcript agree, and that every shape routine is accepted.
