# Sibling top-level blocks reusing a `&`-sigil lexical name no longer corrupt forward-reference resolution

```raku
{
    my &g = -> { f() };
    my &f = -> { 100 };
    say g();
}

{
    my &g = -> { f() };
    my &f = -> { 200 };
    say g();
}
```

Expected (and now matches `raku`): `100` then `200`. mutsu used to print
`100` then `Unknown function: f`.

## Root cause

`Compiler::inherit_outer_code_var_names` (`src/compiler/helpers_sub_body.rs`)
threads the `&`-sigiled names visible at a closure's compile point down to
that closure, so `compute_free_vars` can recognize a bare call `f(...)` as a
read of an enclosing `&f` lexical. It sourced that set from
`self.local_map.keys()` — but `local_map` is a *monotonic* map, deliberately
kept mapped to a name's slot after the declaring scope closes (so a later
*sibling* block can reuse the slot; see `Compiler::pop_local_scope`'s doc
comment). Using it here meant a block reusing an earlier sibling's `&`-name
wrongly inherited that stale slot as "already in scope" — routing the
reference through the compiled-outer-var path instead of the dynamic
name-lookup fallback the *first* (genuinely forward-referencing) sibling
itself correctly used, since at that point `local_map` genuinely didn't
have the name yet.

## Fix

Filter against `self.local_scopes` (the live scope-frame stack, correctly
popped when a sibling block/sub closes — already used elsewhere in the
compiler, e.g. the `MUTSU_SHADOW_SLOTS` shadow-detection logic) instead of
the monotonic `local_map`. A name only counts as a visible outer code var if
some currently-open scope frame actually declares it.

## Tests

`t/sibling-block-code-var-name-leak.t` (new) — the ticket's exact bare-block
repro, the sub-declaration variant, and three separate sibling blocks in a
row. `t/forward-captured-code-var.t`'s note about avoiding shared names
across siblings updated to point at the new pin instead of the (now closed)
ticket.
