# The scoped-overlay return merge stops paying for a flattened env

`call_compiled_function_named_inner` ends every named call with the
scoped-overlay return merge: walk the callee env and copy back the names the
caller also has. On a *scoped* env that walk is O(callee writes), which is the
whole point of `docs/vm-dual-store.md` Slice 6. But any full method dispatch
inside the callee body collapses the overlay
(`todo/perf/method-dispatch-flattens-the-env-on-every-call.md`), and a
flattened env holds the caller's entire lexical scope — so the merge silently
becomes O(whole program scope), on every call whose body touches a method.

The vendored upstream `Test` makes two method calls per assertion
(`$output.say: $tap`, `$desc.Str`), so every assertion paid it twice. Callgrind
put `std::thread::local::LocalKey<T>::with` at **11.8%** of the assertion loop,
with this merge as its dominant caller: **~556 thread-local round trips per
named call**, from resolving each key's symbol to a `&str` two or three times
and re-scanning the bytes.

Three fixes, none of which changes when the flatten happens:

**A memoized flag byte per symbol.** The merge asks three pure, string-derived
questions about every key — is it a routine-scoped implicit (`$!`, `$/`, `$0`,
`$<name>`), is it a per-call-site index-rw temp, is it a `__mutsu_type::`
metadata key. Symbol ids are append-only and a symbol's string never changes,
so the answers are computable once and cached forever. `Symbol::flags()` now
serves all three from a single lookup (`symbol::flags`), replacing three
`as_str()` round trips plus several `memcmp`s per key. `is_callee_local_sym`
takes the same gate before its `strip_prefix`.

**Skip the merge of a key the callee never rebound.** The merge re-inserted
every caller-visible key it walked — a hash write, a refcount bump and a drop
each — even though on a flattened env the overwhelming majority are the
caller's own untouched bindings that the flatten copied in. `Value::same_binding`
(O(1): the same immediate, or the same heap allocation) now short-circuits
those. An in-place container mutation leaves those bits unchanged too, and
correctly so: the caller shares the allocation and already sees it.

**`Env::flattened()` short-circuits an empty overlay.** A scoped tier that never
received a write and holds no tombstone is invisible to lookups, so the
parent's flattening *is* this env's — including `file_sym`, which
`scoped_child` copies from the parent for exactly that reason. Returning
`parent.flattened()` is an `Arc` bump instead of a whole-map clone when the
parent is already flat, with the copy deferred to the first write via
`cow_mut`. Same "an empty tier is not a tier" rule `scoped_child` already
applies when it chains over an empty parent instead of stacking on it.

## Measured

Instructions per assertion, callgrind, 300 `ok 1, "x"` under
`MUTSU_REAL_TEST=1` minus a one-assertion baseline (deterministic, so the
noisy wall clock of this box does not enter):

| | Ir / assertion |
| --- | --- |
| before today | 1.06 M |
| after the multi-resolution cache keys | 658 k |
| after this change | **535 k** |

Wall clock: the 20 000-assertion `ok` loop 3.36 s -> 3.03 s;
`roast/S03-buf/write-int.t` under the real module 29.5 s -> **27.4 s** (median
of eight; 26.5-28.1 s with one 30.4 s outlier). Against a 30 s per-file budget
that is still not a comfortable margin — see
`todo/deep/vendor-real-test-module.md`.

The flatten itself is untouched and still costs: an unsound experiment that
removes it entirely was worth a further ~17% before this change. What is gone
is the *compounding* — the flatten no longer also turns an O(callee writes)
merge into an O(whole scope) one at three string scans per key.
