# Red: a `)` in a trait argument string, and `temp $obj.accessor<key> = v`

Two parser gaps that stopped Red's `t/100-schema-constraints` and `t/102-model-access`
at compile time (both now give `raku`'s 78 / 53 `ok` lines):

- A custom attribute trait's argument list was cut at the first `)` it saw, quotes
  ignored, so `has $!bad is index({ .id }, :opclass('x); DROP TABLE y'))` died
  "Confused". The scan now skips single- and double-quoted strings (a `'` after an
  identifier character is an apostrophe).
- `temp $obj.args<key> = v` / `temp $obj.list[0] = v` (and the bare `temp $obj.args<key>`)
  died "Two terms in a row": the element lvalue was only accepted over a variable or a
  further subscript. The container may now be a method-call result (the shared
  hash/array behind an accessor), which the compiled element save addresses by value.
