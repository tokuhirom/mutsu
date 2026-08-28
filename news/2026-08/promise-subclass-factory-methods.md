# Promise factory methods now respect a user subclass

`class Meows is Promise {}`'s `.new` already produced a `Meows`-typed
instance, but every OTHER Promise-constructing class method
(`.start`/`.in`/`.at`/`.anyof`/`.allof`, and `.then` on an existing Promise)
ignored the invocant's subclass:

```raku
my class Meows is Promise {};
say Meows.start({1}).^name;   # was "Promise", raku: Meows
```

Two independent bugs, plus a third that only surfaced once the first two were
fixed:

1. `promise_class_name` (`src/runtime/methods_collection_ops/socket_inet_proc.rs`)
   already threaded the invocant's class name through to
   `SharedPromise::new_with_class`, but for a *lexically-scoped* subclass
   (`my class Meows is Promise {}`) that name is the raw, ADR-0047-mangled
   internal storage key (`Meows\u{0}<decl-id>`), not the clean user-facing
   "Meows" — this is architecturally correct (every other comparison against
   the same class, `class_mro`, `isa_check`, etc., also operates on the
   mangled form), but nothing stripped it for *display*.
2. `Interpreter::dispatch_caret_name`'s `Promise` arm
   (`src/runtime/methods_introspect.rs`) read that mangled name straight into
   `.^name`'s output, unlike the `Package`/`Instance` arms right above it
   (which both call `user_facing_type_name` first) — so `.^name` rendered a
   stray embedded NUL byte + decl-id. Fixed by routing through
   `user_facing_type_name` the same way. `.WHAT`'s hardcoded `"Promise"`
   literal (same file) was also replaced with the Promise's own
   `class_name()`, though the pre-existing Promise-specific `"WHAT"` handler
   in `methods_promise.rs` already got this right on its own.
3. Once `.^name` displayed the clean "Meows", `.isa(Meows)` and (crucially)
   `nqp::istype($meows_promise, Meows)` still disagreed with it, because
   `Interpreter::dispatch_mro`'s catch-all fallback for a `ValueView::Promise`
   (`src/runtime/receiver_class.rs`) used `value_type_name`, which is
   hardcoded to the literal string `"Promise"` for every Promise value,
   subclassed or not. Added a dedicated `Promise` arm that routes through
   `class_chain(&p.class_name().resolve())` — the same registry-MRO
   mechanism already used for `Instance`/`Package`, and for a user class
   `is Array`/`is List`. This is what actually mattered for the roast
   regression: the vendored real `Test.rakumod`'s `isa-ok` calls
   `nqp::istype($var, $type.WHAT)` for a non-`Str` expected type, not `.isa`
   directly, so this fix was required even though `.isa` alone (fix #1+#2)
   already agreed with `.^name`.

Closes `roast/S17-promise/basic.t`'s "subclasses create subclassed Promises"
subtest under both the native and the real `Test` provider. Regression test:
`t/promise-subclass-factory-methods.t` (green under `raku` too).
