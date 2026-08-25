# Uncaught-exception rendering prints `.message`, never `.gist`

Found while implementing the `X::Promise::Broken` role mixin
(`news/2026-08/promise-broken-exception-not-wrapped-in-x-promise-broken.md`),
but it is a general divergence with nothing Promise-specific about it.

## Repro

```raku
class E is Exception {
    method message { "the-msg" }
    method gist    { "THE-GIST" }
}
E.new.throw;
```

- raku: `THE-GIST`
- mutsu: `the-msg` followed by a backtrace

Rakudo renders an uncaught exception by calling `.gist` on it. mutsu renders
`RuntimeError::message` plus `RuntimeError::backtrace()`, so **any** user
`method gist` override is ignored at the top level, even though `say $ex` and
`$ex.gist` both honour it correctly.

## Why it matters beyond a user override

Several core exception shapes are *defined* by a `gist` override rather than a
`message` one. The one that surfaced this:

```raku
my $p = Promise.new; $p.break('oh no'); $p.result;
```

- raku:
  ```
  Tried to get the result of a broken Promise
    in block <unit> at f.raku line 1

  Original exception:
      oh no
        in block <unit> at f.raku line 1
  ```
- mutsu: `oh no` plus a backtrace

Caught, mutsu is now byte-identical to raku here — `$ex.gist` produces the
wrapper. Only the uncaught path diverges. `X::Await::Died` ("An operation first
awaited: ... Died with the exception:") is the same story.

## Why it is not a small fix

`error_render::render_error` is a pure function over `RuntimeError` in
`src/error_render.rs`, called from `main.rs`. It has no `Interpreter`, so it
cannot dispatch `.gist` — which may run arbitrary user code, and may itself
throw. Doing this properly means rendering the uncaught exception *inside* the
interpreter (where `render_gist_value` already exists) before the error reaches
the CLI, and deciding what to do when `.gist` dies or when there is no
exception object at all (a plain `RuntimeError` from a native failure).

That touches the top-level error path for every program, so it wants its own
slice rather than being smuggled into an unrelated fix.

## Affected files

- `src/error_render.rs` (`render_error`)
- `src/main.rs` (the call site)
- `src/runtime/io_env.rs` (`render_gist_value`, the existing in-interpreter
  gist dispatcher this should reuse)
