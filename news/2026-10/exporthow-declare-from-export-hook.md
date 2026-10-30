# A declarator published by an `EXPORT` hook through `LEXICAL::<EXPORTHOW>`

annotations 0.1.0's `use annotations <declare direct class>` makes an
`annotation` declarator whose name and HOW the `use` arguments choose: the
`EXPORT` hook binds `LEXICAL::<EXPORTHOW> := PACKAGE <EXPORTHOW DECLARE
annotation>, annotations::how[Buffer, $*LANG.how: 'class']`. Pieces that make it
work:

- A module whose hook binds `LEXICAL::<EXPORTHOW>` is run at parse time like a
  slang activation (with the `use` arguments, qw lists included), and the
  declarators it left in the `EXPORTHOW::DECLARE` / `SUPERSEDE` stashes become
  keywords of the importing unit.
- `$*LANG.how(NAME)` / `.know_how(NAME)` answer for the language's own
  metaclasses, and `$*LANG` is bound while the hook's candidates are selected
  (their `where` clauses ask the language).
- A role used as the metaclass (`role AnnotationHOW[::H] is H`) is punned into
  the metaobject; its `compose` runs like a class HOW's.
- `OUR.WHO.BIND-KEY: .key, .value for Other::` in `package EXPORT::MANDATORY`
  exports the copied types and variables, not only the routines.
