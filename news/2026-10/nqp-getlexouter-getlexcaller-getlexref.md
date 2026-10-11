# nqp::getlexouter, getlexcaller, bindlexdyn, lexprimspec and getlexref_*

The second slice of the `nqp::` lexical-introspection ops (#11498) lands seven
more ops, leaving `ctxouter` as the only one of the category still missing:

- `nqp::getlexouter($name)` looks a lexical up from the scope enclosing the
  running one outward. A literal name compiles to the `OUTERS::` read it
  means, so a routine's own binding never answers for its outer namesake; a
  computed name, or one no scope of the compilation declares (`&say`), is
  looked up at run time.
- `nqp::getlexcaller($name)` reads the lexical as the calling frames see it,
  and answers null when none has it.
- `nqp::bindlexdyn($name, $value)` binds a dynamic variable starting at the
  caller, skipping the running frame's own declaration.
- `nqp::lexprimspec($ctx, $name)` reports how a context's lexical is stored:
  1 for `int`, 2 for `num`, 3 for `str`, 0 for an object.
- `nqp::getlexref_i` / `_n` / `_s` answer an `IntLexRef` / `NumLexRef` /
  `StrLexRef`: a native reference whose writes go to the same cell the
  lexical's closures share (the one an atomic op on it already uses).

All the lexical ops now live in their own table, `runtime/nqp_ops_lexical.rs`.

Building `getlexouter` on `OUTERS::` exposed a bug there too: `$OUTERS::x`
inside a routine that declares its own `$x` read that inner binding. It now
reads the enclosing one through the same capture `$OUTER::x` uses.
