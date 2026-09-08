# The END-phaser refresh taxed every closure return, in proportion to the importing scope

`use Test` — the vendored upstream module, `MUTSU_REAL_TEST=1` — made unrelated
hot loops in the *importing* file 2.6x slower on a 4-core box: the loop of
`t/rebound-return-hot-loop.t`, reduced to a file of its own, went from 7.5 s to
19.5 s with nothing but `use Test;` prepended. Issue #7565 had already
apportioned most of that between two known costs — work linear in the mainline
env's size, and the process-global reflective latch that one `EVAL` anywhere
sets forever — and noted that the two together predicted about two thirds of
what was measured. Finding the rest was the first job.

## The third term

It is a block at the end of `call_compiled_closure`, which runs on **every
closure return** once any `END` phaser is registered:

```rust
if self.has_end_phasers() && !data.env.is_empty() {
    let captured_strs: Vec<String> = data.env.keys().map(|s| s.resolve()).collect();
    let captured_names: HashSet<&str> = captured_strs.iter().map(|s| s.as_str()).collect();
    let current = self.clone_env();
    self.update_end_phaser_envs_for_keys(&captured_names, &current);
}
```

`data.env` is the calling closure's captured env, and its width is set by the
scope the closure was *created* in, not by the closure: importing a module with
a wide export list widens every capture made in the importing scope, and the
reflective latch widens it further to a whole-env snapshot. So the loop below
walked a few hundred names per closure call, resolving each interned `Symbol`
back to a fresh `String` and then re-interning it three times over — once for
`dead_keys`, once for the phaser's captured env, once for the live env — on top
of an `Env::flattened()` of the whole live env for the `current` argument.

Upstream `Test.rakumod` registers an `END` (its final plan check) and its
`throws-like` contains an `EVAL`, so a bare `use Test` arms all three
conditions at once. A callgrind diff of the reduced program at 150 and 450
iterations put this one block at **30% of the entire per-iteration cost**, with
`Symbol::intern` — almost all of it from here — at another 21%.

That is why the three costs did not add up: they *multiply*. Measured on the
same box, debug build, medians of three, over a 20 000-iteration loop:

| variant | before | after |
| --- | --- | --- |
| the loop alone | 7.7 s | 7.5 s |
| `+ END { }` | 9.3 s | 7.5 s |
| `+ 60 mainline `our`s` | 10.9 s | 10.8 s |
| `+ 60 `our`s + END { }` | 13.6 s | 10.8 s |
| `+ 60 `our`s + EVAL + END { }` | 16.1 s | 11.7 s |
| `+ use Test` (vendored) | 19.5 s | 14.6 s |

## What changed

`update_end_phaser_envs_for_keys` now takes the captured `Env` itself and works
in interned-`Symbol` space throughout — no `String` is allocated and no name is
re-interned. The membership questions it asks are unchanged, key for key: the
phaser side stays the chain-walking `contains_key_sym` it always used, and the
captured side stays that env's own overlay keys.

Its call site asks a new `end_phasers_watch_any` first. The update is a no-op
unless some phaser captured one of these names, so asking is what keeps the
`clone_env()` flatten — `O(env)` for a scoped frame — off every closure return
in a program that merely declares an `END`. The guard is the loop body's own
precondition minus one lookup, so it can never skip work the loop would have
done.

The same closure-call path asked three string questions about every captured
name: whether it is a dynamic variable (a leading-sigil scan), whether it is
`self`, and whether it is `$_`/`$!`. The first now reads a memoized
`Symbol` flag bit, `DYNAMIC_VAR_ENV_KEY`, alongside the ones the capture filter
already uses; the other two are `Symbol` equality against the well-known
symbols. The predicate the new bit memoizes is lifted verbatim into
`env::is_dynamic_var_env_key`, deliberately *not* reusing the neighbouring
`is_dynamic_var_name`: that one strips at most one `@`/`%`/`&` and so answers
`false` for `$*OUT`, which the built-in dynamics are seeded under. On the
`EVAL`-latched 240-name variant this alone is a 8.8% instruction-count
reduction.

## What is left, and one finding worth recording

The remaining 14.6 s against a 7.5 s floor is the two costs #7565 already named,
and both are now the whole of it. Under the latch the per-closure-*call* capture
merge re-inserts every captured name into the callee's overlay
(`Env::insert_sym` is 25% of the reduced program); with the latch off, closure
*creation* pays `Env::filtered_flat`, which walks every visible key to keep
about twenty-three (26% of the no-latch variant). Neither is `Test`-specific and
neither is addressed here.

Recorded for whoever picks those up: the whole `t/` suite (3860 files, 40903
tests) passes with this refresh removed outright, not merely made cheap. It is
kept because it is not provably dead — the exit-time overlay in
`Interpreter::run` prefers the *live* env value for every non-frozen captured
key, which is what makes the refresh unobservable in all of those tests, but it
does fall back to the captured value for a key that is missing from the exit env
entirely. `t/end-phaser-wide-capture-refresh.t` pins the END semantics that hold
under exactly the conditions that widen a capture, so the capture work still
outstanding cannot quietly change them.
