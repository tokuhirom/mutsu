# The builtin-type catalog's ancestry chains are built once, not per dispatch

`Str.^mro` is a constant. So is `Int`'s, `Bool`'s and every other row of
`builtins/builtin_type_catalog.rs` — the raku-adjudicated table ADR-0019's E1
classifier and the registry both read to answer "what is this receiver's
ancestry". mutsu rebuilt each of those constants from scratch on **every method
dispatch that reached it**: an 88-row linear scan of the catalog with a string
compare per row to find the row, a `Symbol::intern` per ancestor name to turn it
into a chain, and a fresh `Vec`/`Arc` allocation to hold the result.

Measured on `use Test; plan 2000; for ^2000 { ok 1, "x" }` under callgrind (the
benchmark [#7766](https://github.com/tokuhirom/mutsu/issues/7766) is written
against), `receiver_class::catalog_chain_for_name` alone was **the single
largest `Symbol::intern` caller in the program** — 16.5 of the ~49 interns per
assertion, and 1.44% of the run's retired instructions for an answer that could
not change.

## What changed

The catalog now interns each row's `mro` **once per process**, in both shapes
its two consumers want, beside the row itself:

- `builtin_type_mro_syms(name) -> Option<Arc<[Symbol]>>` for
  `Registry::class_mro`/`class_mro_readonly`, whose builtin path hands the chain
  straight back as an `Arc<[Symbol]>` — now a refcount bump.
- `builtin_type_mro_ids(name) -> Option<&'static [TypeId]>` for the E1
  classifier, which splices chains together and so wants to borrow.

The same table doubles as the catalog's name index, so `builtin_type_info` is an
`FxHashMap` probe instead of the linear scan every one of those lookups sat on
top of.

Interning the chain once still left a copy of it per dispatch, because
`dispatch_mro` handed out an owned `Vec<TypeId>`. Every consumer only ever
*reads* the chain — `.first()`, `.iter()`, `&chain` into `resolve_sequence` — so
the classifier's chain type is now `Chain = Cow<'static, [TypeId]>`: borrowed
straight from the catalog for a plain builtin receiver, owned only for the
composed cases that genuinely build something new (an Enum's `[EnumType,
…base]`, a role mixin's role prefix, a user class's registry MRO with a catalog
tail spliced on). No call site needed changing; the deref coercions carried it.

## Measured

Release, callgrind, warm precompilation cache, 2000 assertions, second run after
the rebuild (the first is not comparable — see the ticket):

| | before | after |
| --- | --- | --- |
| `Symbol::intern` calls | 98,096 | 66,805 |
| `Symbol::intern` (inclusive Ir) | 17.84 M (3.75%) | 12.89 M (2.73%) |
| `receiver_class::catalog_chain_for_name` (inclusive Ir) | 6.84 M (1.44%) | 1.31 M (0.28%) |
| `receiver_class::dispatch_mro` (inclusive Ir) | 7.49 M (1.57%) | 1.96 M (0.41%) |
| whole run (Ir) | 475.94 M | 472.88 M (−0.64%) |

−31,291 interns, i.e. **−15.6 per assertion**, the largest single cut to that
budget so far (#7871 took −11.9). `dispatch_mro` — which `type_matches_value`
calls on every typed parameter bind and every smartmatch, not only in this
benchmark — costs a quarter of what it did. Roughly half the whole-run saving is
the interning and the linear scan; the other half is the per-dispatch `Vec` the
`Cow` removes.

## Regression cover

`t/oo/class/builtin-catalog-mro.t` pins the thing this class of change can get
wrong: a shared, pre-built chain that drops, reorders or cross-wires an ancestor
is a *wrong ancestry*, not a slowdown — it changes which class answers a method.
The file checks each catalog row's `.^mro` against raku's, that a repeat read is
unchanged, that two user classes deriving from different builtins each get their
own tail, and that a method augmented onto `Cool` is reachable from `Str`, `Int`
and (through `Int`) `Bool`. `interned_views_agree_with_their_row` does the same
at the unit level for every row in the catalog, both interned views at once.

Two pre-existing `.^`-MOP divergences turned up while writing that file and are
filed as [#7937](https://github.com/tokuhirom/mutsu/issues/7937): `.^isa` on a
*concrete* builtin value answers False (`"x".^isa(Cool)`), and `.^mro` alone
drops a parametrized name's base row (`Array[Int].^mro` stops at `Any`) where
both the classifier and the registry splice it in. Neither is caused by this
change — both reproduce on `main` — so the test uses `.isa` and omits the
`Array[Int]` row, with a comment saying to swap them back.

## Not done

The rest of [#7766](https://github.com/tokuhirom/mutsu/issues/7766)'s unit 2 —
the `_sym` variant chains for `call_compiled_function_named`,
`user_method_overloads`/`has_user_method`, `multi_arg_type_keys` and the
resolution layers — is untouched and still open, re-measured on the issue.
