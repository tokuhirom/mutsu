# `my &f = ...` shadows an outer `sub f` for a bare-name call

A `my &f` binding is an ordinary lexical, so it shadows any package/registry
routine of the same name and a **bare-name** call must reach the binding —
exactly as the explicit `&f(...)` form does.

mutsu compiled the bare name to a by-name `CallFunc`, which resolves against the
routine registry, so an outer `sub f` won. The failure is rarely recognisable as
a scoping problem, because the two routines usually differ in arity:

```raku
sub tester($a, $b) { say "outer $a $b" }
{
    my &tester = -> $x { say "lexical $x" };
    tester(9);
}
```

```
raku : outer? no — "lexical 9"
mutsu: Too few positionals passed; expected 2 arguments but got 1
```

The compiler already knows the answer: `Compiler::amp_binding_in_active_scope`
asks whether `&name` is declared in a lexical scope that is still **active**
here, walking `enclosing_scopes` then `local_scopes`. (Asking `local_map` would
be wrong — it is monotonic, so it keeps names left behind by already-popped
*sibling* blocks and would capture calls no longer in the binding's scope.) When
it is, the call compiles as the `&f(...)` form, which already has its own opcode
(`CallOnCodeVar`). Nothing changes for a name with no such binding, so the hot
by-name call path is untouched.

## The slot-only half, which this exposed

Routing through `CallOnCodeVar` immediately broke `t/named-callable-param.t`,
and the cause was a **pre-existing** gap rather than the new route: a `&`-sigil
binding may live only in a frame's local slot and never in env — which is how a
`&`-sigil *named parameter* binds
(`news/2026-08/named-callable-parameter-binds.md`) — while
`exec_call_on_code_var_op` consulted only `resolve_code_var`, i.e. env. So

```raku
sub takes-cb(:&cb) { &cb.defined ?? &cb() !! 'unpassed' }
say takes-cb(:cb({ 'called' }));
```

answered `Unknown function: cb` on `main` even though `&cb.defined` worked. The
op now falls back to this frame's `&name` local slot, which fixes the direct
`&cb()` call as well as the bare-name one.

## What it unblocks

`roast/S04-statements/given.t` runs 54/54 under `MUTSU_REAL_TEST=1`. Its
`given/when with explicit conditions` group does

```raku
my &test-given = produce-tester $condition;   # produce-tester EVALs a fresh sub
test-given topic, $desc, :$match;
```

while an earlier subtest's `my sub test-given(Mu \topic, Mu \condition, $message, :$match)`
is still registered, so the call reached that 3-positional routine and died with
"Too few positionals passed; expected 3 arguments but got 2", losing the whole
group. It is the third and last of the three general bugs that file exposed,
after `news/2026-08/eval-sub-shadows-a-registered-routine.md`.

Pin: `t/lexical-amp-binding-shadows-a-routine.t` — 9 of its 10 assertions fail
without the fix, and all 10 pass under `raku`.
