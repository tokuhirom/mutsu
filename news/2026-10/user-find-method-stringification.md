# String contexts ask a user `^find_method` for their stringifier

A class with a `method ^find_method` now answers the stringifier calls that `say`, `put`,
`print`, `note`, infix and prefix `~`, interpolation, `eq` and the string comparators,
`join` and a list's `.Str`/`.gist` make internally, as in Rakudo (#10819). `say $obj`
asks for `.gist`; `put`, `print`, infix `~`, `join` and a list's `.Str` ask for `.Str`;
prefix `~`, interpolation and `eq` ask for `.Stringy`. Previously these paths looked only at
the class's own method table, so such a type object warned "Use of uninitialized value"
and rendered empty.

The interception now also covers the runtime's generic method-call entries
(`call_method_with_values`, `try_compiled_method_or_interpret`), not only the
method-call opcodes. A dispatcher Method object (what `.^lookup` returns for a multi)
invoked on a type object is now bound to its owner's candidates instead of re-dispatching
by name, so it cannot loop back into the user's `find_method`.
