# Calling a block-local sub no longer costs 1.7x more than calling a file-scope one

`todo/deep/interpreter-call-path-in-hot-loops.md` recorded a measurement that looked like a bug
rather than a general "calls are expensive" result: a 1M-iteration loop calling a one-line sub was
**1.7x slower when the callee was declared inside the calling block** than when the identical body
was declared at file scope (3.8x with a zero-arg callee). Nothing about invoking a routine should
depend on where it was declared, and it is not a corner case — roast bodies and `Test.rakumod`'s own
helpers declare subs inside blocks constantly, and every `lives-ok`/`throws-like`/`subtest` body is
exactly this shape.

## What was actually happening

A routine declared inside a block *is* compiled ahead of time, but its `compiled_fns` key is
namespaced by the enclosing closure (`GLOBAL::&<closure>/7::inner-fn/1#6367421e45ed0903`), and
bare-name resolution at the callsite probes `GLOBAL::inner-fn/1#...`. The probe misses, so the
routine is compiled again on the fly from its `FunctionDef` AST and dispatched through
`otf_call_cache` — the third of the three name-keyed caches in `exec_call_func_op`, and the one with
no fast path attached to it.

`perf diff` between the two shapes (`taskset`-pinned, `cpu_core/cycles/u`) put `memmove` at the top
of the delta with **+15%** of the whole run, plus fresh `malloc`/`free`, `hash_one` and `memcmp`
traffic that the file-scope shape never paid. The causes, in order of size:

- **The OTF cache moved the body in and out of its `HashMap` around every call.** The hot path did
  `otf_call_cache.remove(&name)` -> call -> `insert(...)` purely to release its borrow on `self`. A
  `CompiledFunction` embeds a whole `CompiledCode` (~1 kB of `Vec`/`HashMap` headers), so that was
  two struct memcpys plus a rehash *per call*. That is the `memmove`.
- **Every call re-derived the callsite analysis.** `is_light_call_eligible`,
  `is_positional_light_call_eligible`, the junction/slip scans, the container-share checks and the
  arg-source decode are properties of the callee (or of the arguments), and the two caches above it
  precompute exactly this — but the OTF path recomputed all of it on every call before landing in
  the very same `call_compiled_function_positional_light`.
- Two smaller ones on the same path: `has_multi_candidates_cached` took the name as a `&str` and
  re-`intern`ed it (a string hash per call) even though the callsite already holds the pre-interned
  `Symbol`, and the argument buffer was a fresh `drain(..).collect()` instead of the pooled
  `Vec<Value>` the light paths use.

## The fix

`pos_light_call_cache` — the ultra-fast positional cache checked first in `exec_call_func_op` — now
holds a `PosLightTarget` that is either a key into `compiled_fns` (as before) or an
`Arc<CompiledFunction>` for an OTF-compiled body, package-keyed the same way `otf_call_cache` is.
The OTF path promotes into it the moment it establishes that the callee is positional-light
eligible, so the *second* and every later call to a block-local sub takes the same single fused
scan + light call that a file-scope sub takes. `otf_call_cache` and `otf_compile_cache` hold
`Arc<CompiledFunction>` too, so releasing the borrow on `self` is a refcount bump and the entry
stays in the table; `has_multi_candidates_cached_sym` takes the pre-interned `Symbol`; and the OTF
path borrows the pooled args buffer.

Dropping the remove/insert also fixed a latent eviction bug: a callee with inner subs failed the
`!cf.has_inner_subs` guard *after* its entry had already been removed, so it was thrown away and
re-OTF-compiled on the next call, forever.

## Result

Retired instructions (`perf stat -e instructions:u`, core-pinned, release, 1M iterations), which are
load-independent and therefore the honest metric on a busy machine:

| shape | before | after |
| --- | --- | --- |
| A `$n = $n + 1` | 2.04 G | 2.04 G |
| B `$n = outer-fn($n)`, callee at file scope | 5.52 G | 5.54 G |
| C same, callee declared **inside the calling block** | **9.20 G** | **5.70 G** |

C/B goes from **1.66x to 1.03x** — the declaration site no longer changes what a call costs. The
general call-path deficit against raku that `todo/deep/interpreter-call-path-in-hot-loops.md` tracks
(rows A and B) is untouched; only the block-local surcharge is gone.

Pinned by `t/block-local-sub-dispatch.t`, which covers what the widened cache could break: sibling
blocks re-declaring the same name, a block-local sub shadowing a file-scope one and the file-scope
one coming back afterwards, the same shape reached through a closure argument, and a block-local sub
closing over a loop variable.
