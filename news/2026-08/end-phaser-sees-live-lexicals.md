# An END phaser sees the final value of a live lexical

Raku's `END` is a closure over its enclosing lexical scope, so it observes the
*final* value of every lexical it mentions. mutsu's `Env` is value-keyed rather
than cell-keyed, so a registered END carries a captured copy of the env instead
of a reference; the exit-time merge in `run.rs` then had to decide, per key,
whether the captured copy or the live env was authoritative. It decided by
comparing values:

```rust
// Overlay with captured value only if the captured value differs from the
// original -- this indicates the captured value comes from a different
// lexical scope (e.g. { my $a = 42; END { ... } }).
if v != orig_v { self.env.insert_sym(*k, v.clone()); }
```

"Different value" is not evidence of "different variable", though — it is just
as easily evidence that the *same* variable was assigned after the phaser was
registered. So any lexical mutated after an END saw its registration-time value
resurrected at exit:

```raku
# EndLive.rakumod
unit module EndLive;
my int $count;
sub bump() is export { $count = $count + 1 }
END { say "count=$count" }
```

`use EndLive; bump; bump; bump;` printed `count=0` instead of `count=3`.

## Where it surfaced

Rakudo's real `Test.rakumod` counts tests in exactly that shape — a module-scoped
`my int $num_of_tests_run` bumped by `proclaim` — and its `END` block re-checks
the count against the plan. Running the unmodified upstream module (step 2 of
`todo/tickets/vendor-real-test-module.md`) therefore ended roughly a seventh of
the sampled `t/` files with a spurious

```
# You planned 9 tests, but ran 6
```

and exit status 255, even though all nine `ok` lines had been emitted with the
right numbers. The `ok` lines were printed from `proclaim`, which read the live
value; only the END block read the resurrected copy. `lives-ok` / `dies-ok` were
the usual trigger, because they are what pushed the counter far enough past the
captured value for the mismatch to be visible.

## The fix

Decide on *identity*, not on value. `EndPhaser` is now a named struct carrying a
`dead_keys` set alongside its body, env and package. A key enters `dead_keys` at
the one moment that actually establishes "the captured copy is the last
surviving binding of this name": when the declaring scope dies. The block-scope
exit path already refreshed captured envs for phasers registered inside the
block (`update_end_phaser_envs`); it now also hands that call the block's
`block_declared` set, which is precisely the names the scope takes with it.

At exit, the captured copy wins only for a key that is absent from the live env
or listed in `dead_keys`. Every other captured name still refers to a live
variable, so the live value wins — including mutations made after registration.
`{ my $a = 42; END { say $a } }` still prints 42 with an outer `my $a = 1` in
scope, because `$a` was frozen when the inner block ended.

Pinned by `t/end-phaser-live-lexical.t`, whose five cases all produce identical
output under `raku`. In the step-2 survey sample (301 `t/` files run against the
vendored upstream `Test`), fully-clean files went from 198 to 255; the
"planned N but ran M" family went from ~40 files to one.

## Still open

An `END` registered inside a sub, closing over that sub's *own* lexical, still
reports the registration-time value: `sub f { my $x = 5; END { say $x }; $x = 7 }`
prints 5 where Rakudo prints 7. That is the same bug family at a different scope
— the named-sub return path has several exits and does not yet freeze/refresh —
and is filed as `todo/tickets/end-phaser-in-a-sub-frame.md`.
