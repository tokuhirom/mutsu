# A hook's term under a computed key binds in the importing block

A module's `sub EXPORT` hook can return a lowercase term under a key the
parse-time scan cannot read, as in `%( $t => Foo.new )` (the shape of
Terminal::ANSI::OO). That term is now kept in the importing block's lexical
import set under its term key, instead of an env alias (ADR-8751, #14140).

- The block's bare-word reads of the name (`GetBareWord`, and the term half
  of `GetExportTermOrJump`) find it through their `LexImportCall` prefix
  (`exec_lex_import_term_site`).
- A closure taken out of the block keeps the term. It used to die with
  `Unknown function: t` once the block's env alias was rolled back.
- A top-level routine the block calls does not see the term, not even
  through an `EVAL`.
- An `EVAL` in the block does find it: its undeclared-name and
  undeclared-routine checks count the open sets' hook terms.
- A block `use` whose set holds hook values is no longer replayed
  (`lexical_import_replay`): the hook runs again, and that run records what
  the block hides from its callees.

The hook helpers moved out of `runtime/lexical_imports.rs`, which had grown
past 500 lines, into `runtime/lexical_import_hooks.rs`.
