# An `is rw` argument reaches a routine with named parameters through `use`

`sub w($pos is rw) { imported($pos) }` lost the callee's write when `imported`
came from `use` and took a named parameter, and `parse-value` -> `parse-integer`
inside a module lost it from the second call on. The by-name call publishes the
caller's shared `is rw` cell to the environment before binding; the
lexical-import route and the cached on-the-fly route called the binder without
doing so, so the binder boxed a fresh cell. Both now publish it first.

Found with TOML, whose `parse-toml` returned `Nil` for any scalar value: all
five of its test files now match rakudo (2, 66, 66, 66 and 102 assertions).
