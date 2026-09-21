# CallMethodMut never consulted the fast method-dispatch cache

A method call on a named lexical variable (`my $o = C.new; $o.m()`) always
compiles to the `CallMethodMut` opcode, not the plain `CallMethod` opcode.
But `fast_method_cache` — the monomorphic per-(class, method) cache that
skips straight to `dispatch_compiled_method`, bypassing `resolve_method_cached`,
`check_method_wrap_chain`, and a full `attributes.to_map()` clone of the
receiver's attribute map — was only ever consulted and populated by the
non-mut `CallMethod` entry (`try_compiled_method_or_interpret_inner`). Every
`CallMethodMut` call re-ran that whole resolve/wrap-chain/clone walk from
scratch, even the second call in a tight loop on the exact same (class,
method) pair, because the cache the sibling call form had already built for
it was never checked.

Found while investigating [#8880](https://github.com/tokuhirom/mutsu/issues/8880)
("a method call costs 22,528 instructions because it re-resolves the method
by name every time"): profiling the issue's own `$o.m()` repro showed the
call actually compiles to `CallMethodMut` (the receiver is a `my $o`
variable), not the `CallMethod` opcode the issue's own analysis targeted —
and `try_compiled_method_mut_or_interpret_sym` had no fast-cache check at
all.

Fixed by porting the identical fast-cache check (same cache, same
eligibility gate, same generation-based invalidation) into
`try_compiled_method_mut_or_interpret_sym`, plus a `try_populate_fast_cache`
call after a successful resolve so later calls actually hit it. No new
cache, no new invalidation surface — this reuses the mechanism the non-mut
path already has and roast already exercises.

Measured on the issue's repro (100,000 iterations of `$o.m()` in a `while`
loop, callgrind, `--cache-sim=no --branch-sim=no`, cold run discarded,
re-baselined from the same `main` commit this branched from):

| | before | after | delta |
| --- | ---: | ---: | ---: |
| total instructions | 2,397,185,869 | 2,319,410,016 | -3.24% |
| per-call (loop overhead subtracted) | 21,792.8 | 21,015.0 | -777.8 (-3.57%) |
| total allocations (100k calls) | 1,321,747 | 1,221,749 | -100,000 (-1/call) |
| total bytes allocated | 49,153,037 | 40,753,301 | -8,399,736 (-84/call) |

The old `call_compiled_method`/`push_method_dispatch_frame`/
`attributes.to_map()` route now runs only on the first (cold) call at a
given call site instead of on every one.

This is one bounded slice of #8880, not the whole thing — the issue's
proposed per-call-site inline cache (and the pre-dispatch probe chain it
would short-circuit) is still open; #8880 stays open for that. Regression
coverage: `t/routines/dispatch/callmethodmut-fast-dispatch-cache.t`
(repeated calls across cache hits, alternating receiver instances, a
method-wrap/unwrap across a warm cache, and a multi method, which
`fast_method_cache` never caches).

## A latent correctness gap this uncovered

Wiring `CallMethodMut` into `fast_method_cache` exposed a real, previously
latent bug in `try_populate_fast_cache`'s eligibility gate: it excludes an
attributive parameter (`$!x`, which mutates `self`) but not a *sigilless
raw-capture* parameter (`\x`), which binds the **argument's own container**,
not a copy. A constructor shaped like

```raku
method !SET-SELF (\v) { $!x := v; self }
method new       (\v) { self.bless!SET-SELF: v }
```

called repeatedly on a bareword/type-object receiver (always
`CallMethodMut`) lost the alias from the second (cache-hit) call onward —
the built instance's `$!x` stopped tracking the caller's container. This is
exactly the shape `roast/S32-list/skip.t`'s ".skip-all and .push-all on
slipping slippy iterators" subtest exercises via a custom `Iterator`, which
is how it was caught: that whitelisted roast test started failing in CI on
this PR.

The gap was already latent on the non-mut `CallMethod` entry too (same
shared cache, same eligibility gate, same `call_compiled_method_fast`) — it
was simply never exercised there, since a bareword-receiver constructor
call always compiles to `CallMethodMut`. Excluding `pd.sigilless` params
from the shared eligibility gate fixes both entries at once. Also hardened:
the `has_attr_aliases` check now scans attribute *values*
(`is_container_ref`) in addition to the existing declaration-time
`__mutsu_attr_alias::` metadata scan, so a runtime-bound alias is caught
even when it isn't the parameter-binding shape above.

Regression coverage: `t/routines/signature/fast-cache-sigilless-param-alias.t`.
