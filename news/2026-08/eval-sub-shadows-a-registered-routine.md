# An EVAL'd `sub` shadows an outer routine — including one only the registry knows about

`EVAL` compiles a new compunit nested inside the caller's scope, so a `sub f`
declared in it **shadows** an outer `f`; it can never be a redeclaration of one.
Only a name declared inside the *same* EVAL still conflicts.

mutsu already had that exemption, but it was wired to one of the two records a
declaration leaves behind. `EVAL_OUTER_AMP_NAMES` snapshots the `&name` env
bindings that existed when the EVAL began, and only the `env`-side redeclaration
check consulted it. The checks against the routine **registry** (`has_single` /
`has_multi`) had no exemption at all.

The two records do not always agree about what exists. A `my sub` declared
inside a block that runs as a *callable* leaves a registry entry reachable after
the block without leaving an `&name` in any visible env tier:

```raku
sub run(&b) { b() }
run { my sub tester($x) { say "outer $x" }; tester(1) }
run {
    my sub produce($n) { EVAL "sub tester(\$x) \{ say \"eval$n \$x\" }" }
    for 1, 2 -> $n { my &t = produce $n; t(9) }
}
```

```
raku : outer 1 / eval1 9 / eval2 9
mutsu: outer 1 / Redeclaration of routine 'tester'. Did you mean to declare a multi-sub?
```

`roast/S04-statements/given.t` is exactly this shape — it EVALs a fresh
`sub test-given` per subtest while an earlier subtest's `my sub test-given` is
still registered — and died on the very first one, taking a whole 3-subtest
group with it.

The fix adds the registry counterpart, `EVAL_OUTER_ROUTINE_KEYS`, taken at the
same point and keyed by interned `Symbol` so the snapshot allocates no strings.
A routine key present in it is shadowed rather than redeclared; a key registered
*inside* the EVAL is not in it and still collides, so the exemption does not
become a blanket "anything goes inside EVAL".

## What the fix unmasked, and had to fix too

`roast/S04-statements/return.t` test 15 was passing **for the wrong reason**:

```raku
is (try EVAL 'my $double = -> $x { return 2 * $x }; sub foo($x) { $double($x) }; foo 42').defined,
   False, 'return is lexotic only; must not attempt dynamic return';
```

The file declares a `sub foo` earlier, so the EVAL used to die of the very
redeclaration this change removes — which made `.defined` `False` without the
snippet's `return` ever being exercised. With the EVAL succeeding, the real
behaviour showed: mutsu returned `84`.

`return` in a non-routine block does a *non-local* return when a routine
lexically encloses it, and throws `X::ControlFlow::Return` when none does. The
EVAL compile path decided that from `!self.routine_stack.is_empty()` — but a
bare `{ ... }` block, a `for` body and a closure all push a `RoutineFrame` too
(`is_block: true`). So an `EVAL` run from inside a mainline block compiled its
snippet as "inside a routine", and a `return` in the snippet's own pointy block
returned from whatever sub later called it. At file scope, where the stack is
genuinely empty, the same snippet was already correct — which is why only the
in-a-block form was wrong.

Both flags the path sets (`is_routine`, `lexically_in_routine`) now ask
`enclosing_routine_exists()`, which ignores block frames. A `return` inside an
EVAL called from a *real* routine still returns from it, as rakudo does. As a
side effect the throw became catchable by the surrounding `try` instead of
escaping to the top level. Pin:
`t/eval-return-target-needs-a-real-routine.t`.

## Two smaller things from the same investigation

* `Env::keys()` exposes only the innermost tier's overlay, while `Env::get()`
  walks the parent chain and the base tier — so the existing `&name` snapshot
  was already missing names the check could find. `Env::visible_keys_where` is
  the chain-walking, key-only counterpart (`flatten()` answers the same question
  but deep-copies every value, far too expensive per EVAL), and the snapshot now
  uses it.
* The underlying leak is still open: a `my sub` inside a block invoked as a
  callable should not be visible after the block at all
  (`::('&tester').defined` is `True` in mutsu, `False` in raku).

Pin: `t/eval-sub-shadows-outer-routine.t` — 5 of its 6 assertions fail without
the fix, and all 6 pass under `raku`.
