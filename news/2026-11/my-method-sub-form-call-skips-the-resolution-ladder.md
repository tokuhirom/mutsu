# A class body's `my method` called in its sub form skips the resolution ladder

`NAME(self, $x)` inside a sibling method, where `NAME` is a class-body `my method`, used to find
nothing in the registry under the short name (the class body's registry restore drops it) and
climbed the whole call ladder -- six full function resolutions, a user-function match, a lexical
`&name` probe -- down to the interpreter terminal's last-resort lookup of the reserved
`__mutsu_class_lexical_method::NAME` env key, about 118k Ir per call (a plain `sub` call is ~9k).
The call now asks that key first (gated by its env flag word, a lexical beats a same-named package
routine, and a nearer `&NAME` slot or a multi/proto family of that name still takes precedence).
This is the `STORE SELF, $topic` call of `annotations`' `Binder.BIND` (#15833).
