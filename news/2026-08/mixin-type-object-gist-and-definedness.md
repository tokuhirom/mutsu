# A mixin on a type object was lost by .gist/.raku, and by :U parameter binding

`.^name` on a role-mixed type object already correctly composed the mixed
name, but `.gist`/`.raku` dropped it entirely:

```raku
my $m = Any but role Meows { method Bool { True } };
say $m.gist;   # was "(Any)", raku: "(Any+{Meows})"
say $m.^name;  # already correct: "Any+{Meows}"
```

Root cause #1: the fast-path method dispatch for a role-mixed value
(`src/builtins/methods_0arg/mod.rs`) had a special case that re-attached the
composed name for `gist`/`raku`/`perl` on a Set/Bag/Mix inner, but every
other inner (including a plain type-object `Package`) fell straight through
to `native_method_0arg(inner, ...)` — delegating to the BARE inner value and
discarding the mixin wrapper, since a type object has no further user-class
dispatch step to fall back into (unlike an instance, which reaches a
separate composed-name-aware retargeting step in
`methods_call_dispatch.rs`). Added a matching fast-path arm for a `Package`
inner.

While chasing why the fix alone didn't close
`roast/6.c/S14-roles/mixin-6c.t` under `MUTSU_REAL_TEST=1`, root cause #2
surfaced: the real `Test.rakumod`'s `is(Mu $got, Mu:U $expected, ...)` multi
(selected because `$m` is a type object, hence undefined) couldn't even bind
`$m` to its `Mu:U $expected` parameter. `Value::isa_check`-level checks
already handled a mixin correctly, but the definedness predicate
`value_is_defined` (`src/runtime/types/mod.rs`) had no `Mixin` arm at all —
every mixin, whether wrapping an undefined type-object `Package` or a defined
`Instance`, fell through to its `_ => true` default and was reported as
*defined*. That's a plain `sub f(Mu:U $x) {...}; f($m)` bug independent of
`.gist`, not specific to Test.rakumod:

```raku
sub f(Mu:U $x) { $x.^name }
my $m = Any but role Meows { method Bool { True } };
say f($m);   # was a signature-binding error, raku: "Any+{Meows}"
```

Fixed by adding `ValueView::Mixin(inner, _) => value_is_defined(inner)`,
mirroring the existing `ContainerRef` arm — a mixin's definedness is exactly
its wrapped value's definedness, nothing more.

Both fixes were needed to close `roast/6.c/S14-roles/mixin-6c.t` tests 48-49
("method/submethod Bool in mixin is used") under `MUTSU_REAL_TEST=1`; the
native `Test` provider's `is` doesn't route through this multi/parameter-
binding path, so it already passed there. Regression tests:
`t/mixin-type-object-gist.t` and `t/mixin-type-object-definedness.t` (green
under `raku` too).
