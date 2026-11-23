# `dd` names elements, aliases and `is rw`/`is copy` parameters

rakudo's `dd` builds its `$name = ` prefix from the argument's container,
`$var.VAR`. mutsu worked from the argument's source variable name alone and
got these cases wrong (#19046):

- An `is rw` parameter now reports the caller's variable (`$x = 5`). The prefix
  is read from the argument's live cell.
- A `:=` alias now reports the variable it aliases, with that container's
  type. This path is shared with `.VAR.name`.
- `dd @a[0]` / `dd %h<a>` now prints `element of @a = 1`, with a typed
  container's element type in front (`Int element of @a = 1`). The compiler
  passes such an argument as its `.VAR` descriptor. An element of an anonymous
  container still prints no prefix.
- An untyped `is copy` parameter's container is now typed `Any`, as on rakudo.
  `dd` shows `Any $p = 1`, and `$p.VAR.of` is `Any`.
