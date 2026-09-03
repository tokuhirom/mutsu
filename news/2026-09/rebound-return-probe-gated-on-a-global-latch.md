# Every routine return stopped asking whether `&return` was rebound

Raku lets `return` be rebound lexically (`my &return = sub ($v) { ... }`), so
both return sites -- the interpreter's `OpCode::Return` arm and the JIT's `ret`
shim -- probed the environment for a `&return` binding before raising the
built-in return signal. The probe was already Symbol-keyed (pre-interned
`wk::rebound_return`, so it did not re-intern the name), but it is a *miss* in
every program that does not rebind `return`, and a miss is the expensive answer:
`Env::get_sym` walks the whole overlay/parent chain and then consults the
immutable global base tier.

A `gdb` breakpoint count on `bench-fib` made the size of it concrete: 3193 calls
produced 6421 `Env::get_sym` entries, i.e. two hashed chain tiers per return,
and every single one of them missed. `perf` put the resulting self time at ~2%,
which turned out to understate it.

The fix follows the pattern the `env.rs` key latches already established
(`CLOSURE_META_KEY_SEEN`, `BOUND_KEY_SEEN`, `PLACEHOLDER_KEY_SEEN`): a
monotonic, process-global `RETURN_REBOUND_SEEN` flag, latched in
`Env::insert_sym`. That is the single funnel every env insert passes through --
`insert`, `insert_through`/`insert_through_sym` and the `entry_or_insert*`
family all delegate to it, and `inner_mut` has no callers -- so no creation site
can slip past it. `set_global_base` latches it too, so the invariant does not
rest on the base tier happening to hold only built-in enum values. Both return
probes now start with `env::return_rebound_possible()`, and the chain walk only
runs once some binding actually exists.

Soundness is the same argument as the sibling latches: a binding can only become
visible to a return by first being inserted, the insert runs earlier in program
order than any return that could observe it, the flag never clears, and an
over-set merely makes the (correct) probe run.

Measured on a release build with a temporary same-binary env switch (the only
reliable cycles A/B on this machine), pinned to one core:

| benchmark | retired instructions |
| --- | ---: |
| `bench-fib` | **-3.94%** (cycles -4.4%) |
| `bench-tak` | **-1.95%** |
| `method-call` | -0.05% |
| `bench-class` | -0.12% |
| `bench-mandelbrot` | +0.08% |

The call-heavy benchmarks are the ones that return often; the rest are neutral,
as expected.

`t/rebound-return-hot-loop.t` grew three cases that pin both sides of the latch
flip: a plain routine returning correctly *before* any `&return` binding exists
in the program, the rebinding taking effect once declared, and the same plain
routine still returning normally afterwards.
