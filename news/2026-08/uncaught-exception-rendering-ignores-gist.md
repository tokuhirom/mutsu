# Uncaught exceptions are rendered with `.gist`, so a `gist` override finally reaches stderr

rakudo's top-level handler prints `$exception.gist`. mutsu printed the
`RuntimeError`'s message plus its string backtrace — which is only ever the
*default* `Exception.gist` — so **every** `method gist` override was ignored at
the top level, even though `say $ex` and `$ex.gist` honoured it correctly.

That is not merely a user-code curiosity. Several core exception shapes are
*defined* by a `gist` override rather than a `message` one, and their
explanatory wrapper never reached stderr:

```raku
my $p = Promise.new; $p.break('oh no'); $p.result;
```

raku, and now mutsu:

```
Tried to get the result of a broken Promise
  in block <unit> at f.raku line 1

Original exception:
    oh no
      in block <unit> at f.raku line 1
```

mutsu previously printed only `oh no` and a backtrace. `X::Await::Died` is the
same story.

## Root causes (there were two)

**1. The renderer had no interpreter.** `error_render::render_error` is a pure
function over `RuntimeError`, called from `main.rs`, so it could not dispatch
`.gist` — which may run arbitrary user code and may itself throw. The fix moves
uncaught rendering *into* the interpreter: the new
`Interpreter::render_uncaught` (`src/runtime/uncaught_render.rs`) reuses the
existing `render_gist_value` dispatcher and `main.rs` falls back to the pure
renderer when it declines. It declines for exactly three things, all of which
`.gist` cannot speak for:

* a parse diagnosis (`err.code()` is set) — the CLI renders those as
  `===SORRY!===` with a source snippet;
* an error carrying no exception object at all (a native failure, a stray
  control signal);
* an exception whose `.gist` itself dies — rather than replacing the user's
  error with the secondary one, the message-and-backtrace rendering that cannot
  fail is used instead. (rakudo's own answer here is a `===SORRY!=== Error while
  reporting exception` followed by an unhandled NQP-level exception and a
  MoarVM backtrace; mutsu deliberately does something useful instead.)

An error surfaced from an unhandled `Failure` renders both stacks — the fail
site and the throw site, joined by `Actually thrown at:`. That join is a
property of the *uncaught throw*, not of the exception (rakudo's `$!.gist` for a
caught one shows only the fail-site frames), so `render_uncaught` re-attaches it
after the gist rather than folding it into `.gist`.

**2. The default `Exception.gist` was wrong for a user subclass.** raku's
`Exception.gist` is the message *plus the backtrace*; `.Str` is the bare
message. mutsu's native `X::*` arm (`builtins/methods_0arg`) already appended
the backtrace, but that arm is **name**-gated on `Exception`/`X::`/`CX::`, so a
`class E is Exception` — recognisable only through the class registry's MRO —
took the interpreter path in `runtime/methods_instance_ops.rs`, which returned
the bare message. Routing uncaught rendering through `.gist` would therefore
have *lost* the backtrace for every user exception class. The interpreter arm
now applies the same message-plus-backtrace rule.

The same name gate hid `.backtrace` itself: `$!.backtrace` on a
`class E is Exception` died with "No such method". It now answers the stamped
`Backtrace` for a thrown exception and `Nil` for one that was merely
constructed, as raku's does.

## Pin

`t/exception-rendering-and-phasers.t` — the uncaught cases assert on a child
process's stderr and exit code (an uncaught exception cannot be observed
in-process). All 21 assertions pass under real `raku` as well as mutsu.

## Files

- `src/runtime/uncaught_render.rs` (new) — `Interpreter::render_uncaught`
- `src/main.rs` — the uncaught call site
- `src/runtime/methods_instance_ops.rs` — default `Exception.gist`/`.backtrace`
  for MRO-identified exception classes
- `src/runtime/exception_message.rs` — `value_is_exception_instance`
