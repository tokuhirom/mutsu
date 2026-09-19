# One `my int` anywhere no longer slows every store in the program

`exec_set_local_scalar_fast` — the fast path a plain `$x = <scalar>` store
takes instead of `exec_set_local_op_inner`'s 600-line cascade — was gated,
among a dozen other lanes, on `env_type_constraint_seen()`. That is a
process-global monotonic latch meaning "has any variable type constraint ever
been registered, on any interpreter". It is the right question for the lane it
was originally written for (skip a `format!` + env probe in the overwhelmingly
common program that declares no typed lexical at all) and the wrong question
for a per-store gate: the first `my int $x` in a program latches it for the
rest of the run, and from then on **every** store of **every** variable in
that program — typed or not, in that routine or not — falls off the fast path.

Two programs differing only by an unused declaration in an already-returned
routine measured 2.45x apart:

```raku
# tmp/lat_off.raku
sub go() { my $i = 0; my $n = 1000000; while $i < $n { $i = $i + 1 }; $i }
```

```raku
# tmp/lat_on.raku — identical, plus:
sub unused() { my int $dummy = 1; $dummy }
```

| | before | after |
|---|---|---|
| no typed lexical anywhere | 0.087s | 0.093s |
| one unrelated `my int` present | **0.221s** | **0.090s** |

The cliff is gone: the second program now runs at the first one's speed. A
`for ^N` loop in the same measurement set went 216ns/iter -> 97.5ns/iter.

## What changed

The store does not need to know whether the *program* has a typed lexical; it
needs to know whether *this name* could carry a constraint.
`ENV_TYPE_CONSTRAINT_NAMES` records that: a process-global, monotonic
1024-bit set of `Symbol::raw() % 1024`, written by
`mark_env_type_constraint_seen_for` at the three `__mutsu_type::*` env-insert
sites — which are the only ones, so a clear bit is trustworthy — and read by
`env_type_constraint_seen_for`. The `SetLocal` fast-path gate,
`var_type_constraint_sym`'s early-out and the constraint-clear path all ask
per name now. The whole-program latch stays in front of each as the cheap
pre-check, so a program with no typed lexical at all still answers without
interning a name.

Both ways the set is imprecise are one-directional. A bit collision (two names
sharing a bucket) and a bit left set after a constraint is dropped (the `None`
arm of `bind_param_type_constraint`, a scope exit) each say "maybe" where the
truth is "no", which only makes a store take the path it already takes today.
Nothing can move a store *off* the cascade that needs it.

`t/vm/typed-lexical-gate-is-per-name.t` pins the semantics the whole-program
answer used to carry incidentally: enforcement and `Bool` -> `Int` coercion on
a native `int` store, `Str` rejection, a typed lexical not leaking its
constraint onto a same-named untyped one, per-iteration typed declarations,
and a native `int` parameter's bind-time coercion. All 14 assertions were
verified against rakudo first.

## Why this was worth finding

It came out of the [#8673](https://github.com/tokuhirom/mutsu/issues/8673)
investigation into `JSON::Fast` parsing a 272KB document ~70x slower than
rakudo. It does **not** fix that workload — `JSON::Fast`'s hot opcode is
`CallFunc` (1.18M of 10.1M opcodes for the 727-record parse), and it executes
`SetLocal` only 6,132 times — but the bisection that found it also established
that the 70x has nothing to do with `JSON::Fast`: a plain
`while $i < $n { $i = $i + 1 }` loop with no JSON and no `nqp::` in sight is
already 84x slower than rakudo (580ns/iter vs 6.9ns/iter), and mutsu retires
roughly 860 machine instructions per bytecode opcode. This latch was the first
of the structural causes to come out of that, and it is the one that is a pure
optimization with no new semantics.
