# `.gist` of a circular container overflows the stack

`.raku` detects a cycle and renders a placeholder; `.gist` walks the cycle until
the process aborts.

```raku
my @c;
@c = 42, @c;
say @c.raku.chars;   # 60 -- fine, the cycle is rendered as a back-reference
say @c.gist.chars;   # thread 'mutsu-main' has overflowed its stack
                     # fatal runtime error: stack overflow, aborting
```

rakudo renders `[42 [...]]` for the gist — it stops at a depth it has already
visited, exactly as its `.raku` does.

A hash cycle and a mixed array/hash cycle behave the same way; only the entry
point differs.

Measured 2026-09-05 against `main` at `e4994a3`, and confirmed pre-existing by
running the repro on a stashed tree while working on
`news/2026-09/renderers-fetch-a-nested-proxy.md`.

## Root cause

`builtins/methods_0arg/raku_repr.rs` carries real cycle detection for the `.raku`
walk — `SEEN_PTRS` / `SEEN_HASH_PTRS`, thread-locals keyed on the `Gc` node
pointer, with `ARRAY_CYCLE_FOUND` / `HASH_CYCLE_FOUND` to decide whether the
top-level rendering needs the `$var = ...` preamble. The `gist` walk has none of
it and simply recurses.

An aborting process is worse than a wrong string: it takes the whole program
down, with no exception a `CATCH` could see.

## Why it is a ticket rather than a one-liner

The mechanism to copy already exists next door, so the *walk* is easy; what needs
deciding is the rendering. `.raku`'s cycle handling produces a name (`@Array_<ptr>`)
because a `.raku` string is supposed to `EVAL` back into an equivalent structure,
and it hoists a preamble to make that work. A gist has no such contract — rakudo
just prints `[...]` at the revisited node — so the two should NOT share the
placeholder, only the visited-set discipline.

Check `.Str`/`~` and `.join` at the same time: they walk elements through the
same pure renderers and are likely to have the identical hole.

Note that `Interpreter::value_has_proxy` (`src/runtime/builtins_lvalue.rs`) got a
visited set for exactly this reason on 2026-09-05, and its comment records the
same `Gc::as_ptr` identity handle — reuse that shape.

## Reproduce

The three lines above, no fixtures.
