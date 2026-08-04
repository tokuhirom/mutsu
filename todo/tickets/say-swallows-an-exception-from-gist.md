# `say` swallows an exception raised while computing `.gist`

`render_gist_value` (`src/runtime/io_env.rs`) calls `.gist` on the value and, on
*any* error, falls back to the native gist:

```rust
match self.call_method_with_values(value.clone(), "gist", vec![]) {
    Ok(result) => Ok(result.to_string_value()),
    Err(e) if e.return_value.is_some() => Err(RuntimeError::controlflow_return(true)),
    Err(e) if /* X::ControlFlow::Return */ => Err(e),
    Err(_) => Ok(crate::runtime::gist_value(value)),
}
```

The fallback is meant for a *dispatch* failure (no `.gist` to call), but it also
eats a genuine user exception thrown from inside `.gist` — including one thrown
while `.gist` forces a lazy `Seq`. The existing `TODO` on that function already
notes that `render_str_value` (`put`/`print`) swallows even the control signals.

Minimal repro — a `die` in a `gather` created inside a routine:

```raku
sub f() { gather { take 1; die "boom-after-take" } }
say f().list;        # raku: dies with "boom-after-take"
say "after f";       # mutsu: prints an empty line, then "after f", exit 0
```

The same gather written at file scope propagates correctly, because `say` gets a
plain `Seq` there rather than an unforced `LazyList` whose `.gist` does the
forcing. `f().raku` also propagates — only the `.gist` route swallows.

This was found while fixing `samewith` inside a lazy `gather`
(`news/2026-08/samewith-inside-lazy-gather.md`); it is what made that bug print
an empty line instead of reporting `samewith called outside of a dispatch
context`, and it is what `todo/tickets/digest-dist-blockers.md` §6 called "a
second problem in how the failing `gather` is sunk".

## Why it is not a one-liner

Narrowing the fallback means deciding which errors are "dispatch failed, use the
native gist" and which are "the user's code threw". mutsu signals a missing
method in more than one shape (a typed `X::Method::NotFound` instance, and plain
`RuntimeError`s whose message starts with `X::Method::NotFound:` — see
`methods_classhow_dispatch.rs` / `methods_grammar.rs`), so the predicate has to
be written against the real set, not guessed. `say`/`note`/`put`/`print` are on
essentially every code path, so the blast radius is the whole suite: the change
wants its own PR and a full roast run, not a rider on an unrelated fix.
