# The compiled regex engine runs `%` and `%%` separators

ADR-0135's compiled regex engine (Slice A, #10251) now compiles separated quantifiers whose atom
and separator capture nothing, such as `\w+ % ','`, `<-[;]>* % ';'` and `\w+ %% [ \s* ',' \s* ]`.

The compiled layout follows the walk's two implementations:

- **Without ratchet** (`for_each_separated_candidate`), every longer chain is tried before a
  shorter one. A chain's `%%` trailing separator is tried before its plain end, and zero
  iterations come last. The first atom may match empty; every later separator-and-atom step has
  to advance, which a new `Advanced` guard checks.
- **Under ratchet** (`match_separated_quantifier_ratchet`), each atom and separator commits to its
  first match and the chain grows while it can. A cut at the chain's end gives nothing back.

Two shapes still decline:

- Captures under a separated quantifier. The walk folds atom and separator captures side by side
  in `append_separated_captures`.
- Frugal separated quantifiers. The walk ignores frugality there, so it disagrees with rakudo
  (#10306), and the compiled engine does not copy that.
