# Native methods need a declared set of accepted named arguments

Follow-on from
[news/2026-08/native-methods-honour-the-implicit-slurpy-named.md](../../news/2026-08/native-methods-honour-the-implicit-slurpy-named.md),
which fixed the *loud* half of "a named argument may occupy a positional slot".
This file records the *silent* half, which the same investigation measured but
could not close soundly.

## What is already fixed

`call_method_with_values` now implements Raku's implicit `*%_` for native
methods: it offers the full argument list first, and only when the whole
dispatch chain answers `X::Method::NotFound` does it retry with the named
arguments removed. That is provably non-regressive — a call that succeeds today
never takes the retry path — and it fixed every case where an unknown named made
the arity-keyed native lookup *miss* (`4.log(:base(2))`, `"abc".uc(:foo)`, and
22 more measured against `raku`).

Re-confirmed 2026-08-26 while closing
`todo/tickets/str-comb-named-arg-only-dispatch-missing.md`: `.comb(:match)` —
which that ticket suspected would need the per-method declaration described
below — is entirely on the *loud* side and is already fixed by the retry. It
needs no accepted-named table. The scope of this ticket is unchanged: only the
silent-wrong-arm cases listed next.

## What is still wrong

The retry cannot help when the wrong arm *hits*. If a native arm accepts the
named `Pair` in a positional slot and numifies or consumes it, the call succeeds
with the wrong answer and there is no error to retry on. Measured 2026-08-25
against `raku` (mutsu on the left of the arrow is the current, post-fix
behaviour):

| Call | raku | mutsu |
|---|---|---|
| `"abc".chop(:zzz)` | `"ab"` | `"abc"` (the pair numified to a 0 char count) |
| `10.polymod(3, :zzz)` | `(1, 3)` | `(1, 3, Inf)` (the pair became a second modulus) |
| `3.fmt("%d", :zzz)` | `"3"` | dies `X::AdHoc` (surplus sprintf argument) |
| `(1,2,3).rotor(2, :zzz)` | `((1, 2),)` | `((1, 2), ())` (the pair became a second cycle spec) |
| `(1,2,3).classify({$_}, :zzz)` | 3 keys | 4 keys — the pair was classified as an element |
| `(1,2,3).first(:zzz)` | dies `X::Adverb` | `Any` |

`chop` / `polymod` / `fmt` have no named parsing at all to extend. `rotor` and
`classify` do, but a named-flavour `Pair` can legitimately arrive there as
*data* — `builtins_collection_classify.rs` says so explicitly in
`callable_item`'s comment ("a list element that is a named-marker `Pair` is data
here, not a call-site named argument") — so blanket-dropping named pairs in
those two would trade one wrong answer for another. `first` is different again:
Rakudo *validates* its adverbs and throws `X::Adverb` for an unknown one, rather
than swallowing it.

## Why it is large

The sound fix is the declaration the ticket that started this predicted: each
native method needs to state **which named arguments it accepts**, so unknown
ones can be dropped before any positional-slot interpretation while declared
adverbs keep flowing through. Today that knowledge is scattered and implicit:

- adverb parsing lives in per-method Rust helpers (`SplitOpts::from_args`,
  `split_string_match_args`, `extract_extrema_adverbs`, `native_comb_method`,
  `dispatch_rotor`, the `subst`/`trans`/`match`/`grep`/`first` handlers, the IO
  and temporal constructors, …), each with its own hand-written match on key
  strings;
- the arity cascade (`native_method_0arg`/`_1arg`/`_2arg`) has no notion of
  named arguments at all — it just indexes `args[0]`, `args[1]`;
- a handful of adverb-aware natives are already lifted out in front of the
  cascade as interceptors (`native_contains_with_options`,
  `native_prefix_suffix_with_options`, `native_substr_eq_with_options` in
  `src/vm/vm_native_dispatch.rs`), which is the shape the end state wants.

Two plausible designs, both needing an ADR before code:

1. **Per-method accepted-named table.** A `native_method_accepted_nameds(method)
   -> &'static [&'static str]` consulted at the dispatch entry; unknown nameds
   are dropped, declared ones stay in place. Cheap to implement, but the table's
   *completeness* is load-bearing: a method missing from it silently loses a real
   adverb. It must be generated from, or checked against, the helpers above —
   never hand-maintained on its own.
2. **Every adverb-aware native becomes an interceptor.** Extend the
   `native_*_with_options` pattern until no arm inside the arity cascade reads an
   argument `Pair`, then make the cascade named-blind by construction. More work,
   but the invariant is then structural rather than a list someone has to keep
   in sync.

Either way the entry point is one place —
`Interpreter::call_method_with_values` in
`src/runtime/methods_call_dispatch.rs`, which every dispatch chain funnels
through exactly once (verified with `rust-gdb`), and which already owns the
implicit-`*%_` retry.

## Repro

```
raku -e 'say "abc".chop(:zzz); say 10.polymod(3, :zzz); say 3.fmt("%d", :zzz)'
# ab / (1 3) / 3
./target/debug/mutsu -e 'say "abc".chop(:zzz); say 10.polymod(3, :zzz); say 3.fmt("%d", :zzz)'
# abc / (1 3 Inf) / dies
```

## Affected files

- `src/runtime/methods_call_dispatch.rs` (`call_method_with_values`)
- `src/builtins/methods_0arg/`, `src/builtins/methods_narg/` (the arity cascade)
- `src/vm/vm_native_dispatch.rs` (the existing interceptor pattern)
- the per-method adverb helpers listed above
