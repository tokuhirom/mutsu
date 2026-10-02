# A user `method ^find_method` intercepts every method call

A class declaring `method ^find_method(Mu \type, Str:D $name)` now answers every method
call on its type, as in Rakudo: `$obj.foo(...)` asks the metamethod for `foo` and invokes
the Callable it returns with the invocant and the arguments, for undeclared and declared
methods alike (metamethod calls and `.WHAT`-style macros keep their own path). This is
what Object::Trampoline, and through it Object::Delayed, are built on (#10804).

Three pieces made the Trampoline shape run:

- A `method` declared in a routine body of a class (`multi method handler` inside
  `method ^find_method`, or inside a `sub` of the class) is installed in the class,
  closing over the routine's latest invocation.
- `proto method NAME(...) {*}` used as a term (`my constant &proto-handler = ...`)
  evaluates to that proto, the dispatcher of the candidates declared beside it.
- A raw invocant (`multi method handler(Object::Trampoline:D \SELF: |args)`) receives
  the caller's container through the intercepted dispatch too, and
  `nqp::assign(SELF, $object)` writes through it, so the proxy replaces itself in the
  caller's variable with the real object.

Calling a multi method's dispatcher Method object (`Foo.^lookup('h')($obj, 2)`) also
picks its candidate with the invocant's `:D`/`:U` constraint now, instead of the first.
