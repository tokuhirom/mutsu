# A sunk statement value asks its type's `^find_method` for `sink`

Rakudo sinks a statement's value through the receiver's metaobject, so a class with a user
`method ^find_method(Mu \type, Str:D $name)` sees `sink` like any other method name
(Object::Trampoline's `slack { Foo.new }` as a statement creates the real object). mutsu's sink
paths only ran a *declared* `sink` method.

- `OpCode::SinkPop` and `sunk_call_result_runs_sink` now also treat a value whose type declares a
  `^find_method` as having a `sink` (`Interpreter::user_find_method_declares`) and dispatch it
  through `try_user_find_method_dispatch`.
- An `is raw` routine whose last statement is a plain call (`nqp::create(T)`,
  `nqp::p6bindattrinvres(...)`) hands back a fresh object rather than a container, so its caller
  sinks the value (`RoutineBodyFacts::result_is_fresh_call`); a raw routine returning a variable
  or anything else is still not sunk.
- A statement call of an imported routine used to compile to a plain `SinkPop`; it now compiles to
  `SinkPopCall` too, which finds the callee through the `LexImportCall` binding (a `sub EXPORT`
  hook's `'&slack' => &trampoline`) as well as the registry (#16331). Object::Delayed's
  `t/01-basic.rakutest` is now 17/17, like `raku`.
