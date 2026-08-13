# `.cache`/`.List`/`.Array` on a lazy Seq now report the coerced type, not `Seq`

Rakudo's `.cache` on a genuinely-lazy `Seq` stays lazy — it reifies and caches
elements on demand rather than forcing the whole list — but the *reported
type* changes to `List` immediately, exactly like the existing `.List`/
`.Array` coercions. mutsu's `LazyList` already carried an
`in_list_context()`/`in_array_context()` marker (added for `t/eqv-lazy.t`)
that `.WHAT` read correctly, but `.^name` (`value_type_name` in
`src/runtime/utils/type_misc.rs`) never consulted it, and `.cache` itself
never set the marker at all — so `$seq.cache.^name` stayed `"Seq"` even
though `$seq.cache.WHAT.^name` correctly said `"List"`.

This was not cosmetic. The vendored upstream `Test.rakumod` dispatches
`is-deeply` through a Seq-narrowing candidate:

```raku
multi sub is-deeply(Seq:D $got, Seq:D $expected, $reason = '') is export {
    is-deeply $got.cache, $expected.cache, $reason;
}
```

Since `$got.cache` still reported `Seq:D`, this multi re-dispatched to
*itself* forever. Under `MUTSU_REAL_TEST=1`, `t/io-cathandle-lazy.t` did not
fail — it died with a Rust stack overflow and dumped core.

Fixed by:
- `value_type_name` now checks `LazyList::in_array_context()` /
  `in_list_context()` before falling back to the gather/non-gather default,
  mirroring what `.WHAT` (`dispatch_what`) already did.
- The four `.cache` call sites (`vm_call_method_mut_ops.rs`,
  `vm_call_method_ops.rs`, `methods_call_dispatch.rs`,
  `builtins/methods_0arg/collection.rs`) now tag the result with
  `with_list_context()` in addition to the existing
  `with_cached_no_sink()`.
- The laziness-preserving-coercion fast path in `vm_native_dispatch.rs`
  (which returned a lazy map/grep pipeline unchanged for `Seq`/`List`/`list`/
  `Array`/`cache`/`values`/`lazy`) now branches per method: `Array` sets
  array context, `List`/`list`/`values` set list context, `cache` sets both
  list context and the no-sink cache marker; `Seq`/`lazy` stay unchanged.
- The `.List` arm in `builtins/methods_0arg/coercion.rs` for an explicitly
  `.lazy`-marked list now tags list context too, instead of returning the
  value untouched.

`t/io-cathandle-lazy.t` no longer aborts under `MUTSU_REAL_TEST=1` (2 of its
9 subtests still fail — a separate, narrower bug: `IO::CatHandle.handles` is
wrongly lazy and wrongly reports `Array`, tracked in
`todo/tickets/cathandle-handles-wrongly-lazy-array.md`).

New regression test: `t/lazy-seq-cache-list-name.t`, covering the
sequence-spec, lazy-pipe, and `lazy`-marked-list flavours of `LazyList`, plus
an `is-deeply(Seq, Seq)` non-recursion check.

Closes `todo/deep/cache-on-a-lazy-seq-must-not-answer-seq.md`'s gap 1 (gap 2
split out to its own ticket above).
