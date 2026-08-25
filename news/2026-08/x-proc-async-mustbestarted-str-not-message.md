# The whole `X::Proc::Async::*` family renders its real message now

```raku
Proc::Async.new('echo', :w).say(42);
CATCH { default { put .^name, ': ', .Str } };
```

- rakudo: `X::Proc::Async::MustBeStarted: Process must be started first before calling 'say'`
- mutsu (before): `X::Proc::Async::MustBeStarted: X::Proc::Async::MustBeStarted`

## Root cause

Not a missing per-type `.message` override, as the ticket guessed — it was one
shared error builder poisoning the generic path. Two identical
`proc_async_error` closures (in `src/runtime/native_proc_async.rs` and
`src/runtime/native_methods/proc.rs`) did:

```rust
let message = class_name.to_string();
ex_attrs.insert("message".to_string(), Value::str(message.clone()));
```

`.message` / `.Str` / `.gist` all check for a `message` *attribute* first and only
fall back to `format_exception_message()` when there is none. Storing the class
name there shadowed the formatter for every single `X::Proc::Async::*` type.

## The fix

`proc_async_error` is now one shared function that stores only the exception's
genuine Raku attributes (`method` / `handle` / `use`) and takes its human-readable
text from `format_exception_message()` — the same table `.message` / `.Str` /
`.gist` consult. New arms were added there for all eight types, so a thrown
exception and a hand-built `X::Proc::Async::MustBeStarted.new(:method<say>)` now
render identically.

The `use` attribute of `X::Proc::Async::BindOrUse` is populated at each throw
site with the phrase Rakudo uses, which differs by *how* the stream was already
claimed (`use :w`, `get the stdout Supply`, `get the output Supply`).

## Verification

A 15-case scenario matrix (every way each of the eight types can be thrown, plus
hand-built instances) was run under rakudo 2026.06 and mutsu; the outputs are now
byte-identical, including the uncaught-exception rendering. Pinned by
`t/proc-async-divergences.t`.
