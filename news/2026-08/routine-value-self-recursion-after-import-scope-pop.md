# A captured `&name` reference to a popped proto/multi import dies instead of stack-overflowing

```raku
my (&plan) = do {
    use Test;
    (&plan)
};
plan(5);
```

Confirmed live against `roast/S32-list/skip.t` under `MUTSU_REAL_TEST=1`
(the real vendored `Test.rakumod`, whose `BEGIN my (&plan, &subtest, ...) =
do { use Test; (...) }` is exactly this shape): mutsu crashed with
`thread '<unknown>' has overflowed its stack` instead of raising a
catchable error.

## Root cause

`plan` in the real `Test.rakumod` is a `proto`/`multi` sub. Reading the
bare `&plan` inside the `do {}` block resolves to a
`ValueView::Routine { package: "GLOBAL", name: "plan" }` — a *name-based*
reference, not a bound closure, since a proto/multi has no single candidate
to point at. `my (&plan) = do { ... }` binds this Routine value into the
outer `&plan` local.

Once the `do` block's import scope pops (existing, correct mechanism),
`GLOBAL::plan` is removed from the proto tables. Calling `plan(...)`
afterwards: `call_sub_value` gets the captured `Routine` value, none of its
registry checks resolve it, and it unconditionally falls back to
`call_function(&name_str, args)` — re-dispatching "plan" *by name*.
`call_function` doesn't find it either, falls through to
`call_function_fallback`, which checks `env.get("&plan")` — finds the
*same* `Routine` value bound to the outer `&plan` local — and calls it
again. No base case, so it recurses forever instead of raising an error.

## Fix

`call_function_fallback`'s env-based callable lookup (`src/runtime/
builtins_operators_fallback.rs`) now recognizes a "dead-end self-
referential" `Routine` value — one whose own `(package, name)` offers
nothing new to try beyond re-searching the exact same name under the same
unqualified/`GLOBAL` package that already failed to resolve it — and skips
redispatching it, falling through to the normal "not found" error path
instead. A `Routine` naming a genuinely different, qualified package is
NOT excluded, since `call_sub_value`'s package-qualified branch may still
resolve it there.

This does not make the construct actually work — `raku` itself reports
`Undeclared routine` at compile time for the equivalent construct, since it
resolves names lexically — it only ensures mutsu fails with a normal,
catchable runtime error instead of crashing the process.

## Tests

`t/routine-value-self-recursion-after-import-scope-pop.t` (new), with a
dedicated fixture module (`t/lib/ProtoRecursionFixture.rakumod`) exporting
a `proto`/`multi` pair under a name that doesn't collide with any mutsu
builtin/listop (unlike e.g. `head`, which would sidestep the buggy path
entirely). Asserts the subprocess exits with a normal positive error code
(mutsu's `Proc` reports a crashed child's exit code as `-1`, not the
128+signal a shell would) and that stderr no longer contains the
stack-overflow abort message.
