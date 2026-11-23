# Statically refuted calls fail at CHECK time

`say 1; sub f(Int $x) {}; f("a")` used to print `1` and then die with
"Calling f(Str) will never work with declared signature (Int $x)" when the
call ran. rakudo's optimizer proves such a call can never bind and rejects
the unit at CHECK time: nothing runs, and the error is a `===SORRY!===`
pointing at the call. mutsu now does the same for the calls it can judge
exactly, before the mainline runs (#18874):

- **The callee:** a plain `sub` declared once at the unit's top level, never
  referred to as `&name`.
- **The arguments:** literals only, none of them named.
- **The refutation:**
  - too few or too many positionals;
  - a required named parameter with no named argument;
  - a literal that fails a core nominal type, or the `Positional` /
    `Associative` of an `@` / `%` parameter.

A parameter with a default, an `is rw` parameter, a `:D` smiley and a
coercion type are left to the run-time binder, as rakudo leaves them. The
signature in the message now shows a `where` constraint as rakudo does
(`(Int $x where { ... })`).
