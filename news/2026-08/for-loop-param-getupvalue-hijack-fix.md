# A `for`-loop's single scalar param no longer loses to a same-named captured outer lexical inside a closure

A single-param `for` loop's parameter only got a compiled local slot when
the name already had one (an enclosing `my` of the same name in the SAME
compiled unit). Without a slot, the parameter was an env-only binding, and
its per-iteration write happens inside the `ForLoop` opcode's own exec —
not a compiled name-write op the free-variable scanner recognizes. So a
pure body read of the param name looked, to `compute_free_vars`, like a
FREE variable — and when the loop lived inside an escaping closure,
`compute_upvalues` rewrote that read to `GetUpvalue`, which resolves
against whatever the closure had captured under that name from an
*enclosing* scope, bypassing the loop's own per-iteration binding entirely.

```raku
sub make() {
    my $i = -1;
    my @parts = 1,;
    for 1..3 { $i++ }
    -> {
        for @parts -> $i {
            say "i=", $i;
        }
    }
}
make()();
```

raku prints `i=1` (the iteration value); mutsu printed `i=2` (the outer
counter's value when the closure was created) — `$i` here is not even a
free variable of the closure at all; the inner loop declares its own `$i`.

Real-world failure: Cro::HTTP::Router::LinkGenerator's `signature-to-sub`
builds an index counter (`my $i = -1; for $s.params[] { ...; $i++; ... }`)
and returns a closure containing `for @fn-parts -> $i { @result[$i] = ... }`
— every call saw `$i` frozen at the counter's final build-time value
instead of each iteration's own value, producing wrong generated URLs
(`http-router-named-urls.t` "Escaped named param" / "Escaped positional").

## Fix

A plain-scalar `for`-loop parameter is now recorded in a new
`CompiledCode::for_loop_param_syms` set (mirroring the existing
`my_declared_enum_sym` precedent for a `my enum`'s bareword bindings) and
subtracted from `free_var_syms` in `compute_free_vars`. The loop param is
never a free variable this closure needs to capture, so it is no longer
rewritten to `GetUpvalue`, falls back to the ordinary by-name env read, and
correctly sees whatever the `ForLoop` opcode wrote for the current
iteration. As a side effect this also closes the adjacent write-through
risk the original diagnosis flagged (a loop reusing an outer name that had
been boxed into a shared cell by another closure no longer captures that
cell at all, so there is nothing for the per-iteration write to corrupt).

An earlier attempt gave the loop param an actual local slot (via
`alloc_local`, reusing an existing same-named slot or allocating a fresh
one) instead of the free-var exclusion above. That looked sound in
isolation but regressed `t/for-multi-param-shared-lane.t`: mutsu's compiler
uses one flat, non-block-scoped local table per compiled unit, so two
*unrelated*, sequential top-level `for @a -> $i {...}` statements sharing
the name "i" ended up aliasing the *same* physical slot once the first one
was given a persistent allocation — a `start {}` closure capturing the
second loop's `$i` by reference then observed the first loop's late
overwrites. The free-var-exclusion route sidesteps this: it institutes no
new local storage or slot lifetime, only tightens which names a closure is
allowed to treat as free.

New pin: `t/for-loop-param-getupvalue-hijack.t`.

## Effect

- `http-router-named-urls.t` (Cro::HTTP suite, not in `roast/`): "Escaped
  named param" / "Escaped positional" now pass (the file's separate rc=124
  timeout is unrelated and still open).
- No regression in `roast/S04-statements/for*.t`,
  `roast/S03-binding/closure.t`, `roast/S06-signature/closure-*.t`, or the
  local `t/for*.t` / `t/*closure*.t` / `t/*loop*.t` clusters (110 files /
  906 tests).
