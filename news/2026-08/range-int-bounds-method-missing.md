# `Range.int-bounds` gained its two-argument candidate, and the one-argument candidate got its real rule

`Range` has two `int-bounds` candidates in Rakudo:

```
multi method int-bounds(--> List:D)                      # (from, to), fails when there are none
multi method int-bounds($from is rw, $to is rw --> Bool) # binds the bounds, answers whether there are any
```

mutsu implemented only the first, and got its semantics wrong for a non-`is-int`
Range. `(3..5).int-bounds(my $min, my $max)` — the form the documentation uses as
an `if` condition — died with `No such method 'int-bounds'`.

## The rule, re-derived against `raku`

The old `GenericRange` arm rounded the lower endpoint *outward* (`ceiling`), so
`(1.1..5.2).int-bounds` happily answered `(2, 5)`. Rakudo does not do that: a
fractional lower bound means the Range has no integer bounds at all. Probing
`raku` v2026.06 over the whole endpoint matrix gives one uniform rule, which
also subsumes the `is-int` fast path:

* both endpoints must be finite Reals — `1..Inf`, `-Inf..5`, `1..*`, `NaN..NaN`
  and `'a'..'z'` have no integer bounds;
* the **lower** endpoint must already be integral (`1.1..5.2` and `1.5..^5` are
  `False`; `1.0..5.0` and `1e0..5e0` are `True`);
* `from = min.floor + excludes-min`;
* `to = max.floor`, minus one more only when the max is excluded *and* integral —
  so `1..^5.0` is `(1, 4)` but `1..^5.5` is `(1, 5)`, and `-5..^-1.5` is
  `(-5, -2)` (a floor, not a truncation).

That rule now lives in one place, `src/builtins/range_bounds_int.rs`, shared with
`Range.minmax`. The `i64::MIN`/`i64::MAX` open-end sentinel keeps its existing
XOR treatment: it means "open end" only when it appears alone, so
`int64.Range.int-bounds` still answers the genuine full-i64 pair.

## Where the two-argument candidate lives

Writing into the caller's containers needs both `&mut Interpreter` and the call
site's argument-source names, which the pure `native_method_*arg` cascade has
neither of. Rather than add a `runtime/methods.rs` slow-path handler, it is
served from the VM's own native dispatch (`src/vm/vm_range_int_bounds.rs`,
hooked into `try_native_method_raw`). It reads `pending_call_arg_sources` — the
same metadata an `is rw` parameter writeback uses — assigns through
`Env::insert_through` so an aliased slot keeps its container identity, and
queues each name for the call site's writeback drain so the caller frame's local
slot is refreshed too. A Range with no integer bounds leaves both arguments
untouched and answers `False`, as raku does.

Pinned by `t/range-bounds-and-rotor.t`, which passes verbatim under both `raku`
and mutsu.
