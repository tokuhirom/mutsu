# A slang's own routine declarator parses its method body in its own language

EEL 0.3 declares `pattern NAME(...) { ... }` inside its `event` classes: the
`routine-declarator:sym<pattern>` token reads a method with rakudo's `method-def`, and the
slang's `method statementlist` parses the body in EEL's pattern language only while
`$*BLOCK ~~ RakuAST::Method`. Its action then mixes a role into `$method.meta-object` and
stores the compiled definition there. Several pieces of the slang host were missing for that
(#16888):

- the statement list a routine body starts now runs with `$*BLOCK` bound to the routine being
  declared, with its name and written signature (a bare block's lists get a `RakuAST::Block`);
- a `routine-declarator` production's replayed action runs after the method is installed, also
  in a class body, so `$method.meta-object` is the class's own method object;
- a method a slang production declares in a class body is installed where it stands, not
  hoisted to the end of the body as if it were nested in a block;
- an `EXPORT` hook's `&make` stays the importing unit's: the slang's own modules, which run in
  the host, keep the core `make`;
- `RakuAST::Literal.from-value` holds any value (an object, a type object, `Nil`),
  `VarDeclaration::Simple` answers `lexical-name` and `set-initializer`, and
  `Var::Lexical.set-resolution` resolves a hand-built lookup;
- `rakudo`'s `DECLARATOR` action is on the host's base actions.

On the way, a regex fix: a bracket inside a string literal of a code block
(`<?{ $<open>.Str eq '[' }>`) no longer closes the `[ ... ]` group around it.
