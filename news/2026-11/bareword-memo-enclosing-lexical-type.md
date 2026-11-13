# A bareword naming an enclosing scope's lexical type is remembered

A bareword type name (`Foo.new`, `$x ~~ Foo`) is resolved through the whole
term chain at run time; the per-site memo (ADR-0121 D3) remembers the answer
for one registry generation. It refused a lexical (`my`) type declared in any
scope but the unit's own: a role or class body's `my class`, read from that
role's methods, was resolved from scratch on every execution — ~15k
instructions each. AttrX::Mooish's `AttrProxy`, `NO-VALUE-YET` and its
role-body `my enum PvtMode` keys were resolved ~20 times per lazy-attribute
access.

A lexical type, or a bare enum key, is now remembered when either

- `env` binds the name to exactly that type or value (the declaration's own
  binding), re-checked on every hit, or
- the chunk declares no type of its own. The declaring block then cannot
  begin or end inside the chunk, so every site of the chunk spelling the name
  sits on the same side of it and resolves it alike, as rakudo's compile-time
  resolution does. The answer is computed once per chunk.

A chunk that declares a same-named type keeps resolving per site, so a
block-local `my class X` still shadows only inside its block.

While checking this, #17711 was filed: a closure's bareword type name is
resolved in the caller's scope rather than where the closure was written (the
same on main before this change).
