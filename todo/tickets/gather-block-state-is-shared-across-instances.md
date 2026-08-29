# `state` inside a `gather` block is shared across gather instances

A `state` variable declared inside a `gather { ... }` body should belong to the
gather **instance** (each evaluation of the `gather` expression makes a fresh
block closure, and `state` is per closure clone). mutsu shares one cell across
every gather created from the same literal.

## Repro

```raku
sub make() {
    return gather {
        state $n = 0;
        $n++;
        take $n;
    };
}
my @a = make();
my @b = make();
say "a={@a.join(',')} b={@b.join(',')}";

my @seen;
for ^3 {
    my @g = gather { state $m = 0; $m++; take $m; };
    @seen.push: @g[0];
}
say "loop={@seen.join(',')}";
```

| | raku v2026.06 | mutsu |
|---|---|---|
| first line | `a=1 b=1` | `a=1 b=2` |
| second line | `loop=1,1,1` | `loop=1,2,3` |

## Notes

Found on 2026-08-29 while adding `Interpreter::gather_compile_cache` (the
`gather` body used to be re-compiled on every creation). **The divergence
predates that cache and is independent of it**: mutsu compiled the body fresh
each time and still shared the state cell, so the state key is resolved by NAME
in the env rather than by the compiled chunk. Verified before and after the cache
landed — identical wrong output both ways.

The map/grep loops solve the analogous problem by scoping state to the closure
instance (`vm.state_scope_id.set(Some(data.id))` in
`runtime/resolution_map_grep.rs`, with the comment "Scope `state` variables to
the closure instance"). The gather forcing path has no equivalent. That is the
shape of the likely fix — give the `LazyList` an instance id and set
`state_scope_id` around the forcing run — but it needs checking against a
`gather` that is forced lazily and resumed (the coroutine path), where the scope
has to be re-established on each resume, not just on the first pull.
