# Every routine return re-interned `&return`

`return` can be lexically rebound (`my &return = sub ($v) {...}`), so both the
interpreter's `OpCode::Return` arm and the JIT's `ret` shim probe the
environment for a `&return` binding before raising the return signal. Both
probes were written as `env().get("&return")` — and `Env::get(&str)` interns
its key, which is a thread-local, string-keyed hash lookup.

That put `Symbol::intern` on **every single return out of natively-compiled
code**. In a `bench-fib` profile (fib(25), JIT on, P-core `cycles/u`), a
call-graph record attributed it exactly:

```
mutsu_jit_1
  mutsu::vm::vm_jit_helpers::ret
    mutsu::symbol::Symbol::intern
      std::thread::local::LocalKey<T>::with     4.23%
```

`LocalKey::with` was 5.3% of self time overall, plus its share of
`__memcmp_avx2_movbe` (1.1%) from comparing the key inside the intern cache.

The name is now a well-known symbol (`crate::symbol::wk::rebound_return`) and
both sites use `get_sym`, so the probe is a chain walk on a `u32` key with no
interning at all.

## Measurement

Interleaved A/B of two release builds, nine alternating runs each, median
retired user cycles on a pinned P-core:

| benchmark | delta |
| --- | ---: |
| `fib` | −6.8% |
| `bench-fib` | −6.6% |
| `bench-tak` | −4.8% |
| `method-call` / `bench-class` / `bench-ctor` / `bench-hash` | +0.2% .. +0.8% |

Both orderings were measured on `bench-fib` and `bench-hash`; both signs
flipped with the swap, so the sub-1% readings on the untouched benchmarks are
binary layout, not a real cost.

`t/rebound-return-hot-loop.t` pins that the rebinding still takes effect after
the routine has gone hot enough to be natively compiled, and that a sibling
routine without the rebinding is unaffected.

This is the third instance of the same shape (after the ADR-0037 routine-frame
symbols and the `"_"`/`"Any"` parameter-bind names): a fixed, known-at-compile-
time name being re-interned on a per-call path. `todo/perf/late-august-call-path-slowdown-remainder.md`
now tracks the 75 remaining `env().get("<literal>")` call sites, which need a
profile of a non-`fib` benchmark to rank.
