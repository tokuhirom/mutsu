# Grammar and RakuAST constructor tables regenerated on Rakudo 2026.11

The three tables mutsu derives from rakudo's grammar and RakuAST constructors
were still the Rakudo 2026.09 snapshots, though the pinned oracle
(`.rakudo-version`) is 2026.11. They are now regenerated on 2026.11 (#19274):

- `src/parser/stmt/simple/core_grammar_rules.txt` gains the colon-free core
  rules `claim-leading-doc`, `enter-package-body`, `leave-package-header` and
  `vet-c-style-for`, so a slang overriding one of them is recognised as
  changing the core grammar rather than declaring its own helper (ADR-16888).
  The table now has a generator, `scripts/gen-core-grammar-rules.raku`,
  instead of a one-liner in its header.
- `src/runtime/rakudo_grammar_productions.txt` gains the productions
  `claim-leading-doc`, `enter-package-body` and `leave-package-header`, so a
  slang actions method of one of those names is treated as a production
  handler, as rakudo calls it.
- `src/rakuast/ctor_names.tsv` gains the constructors of the new
  `RakuAST::VarDeclaration::Implicit::HeaderValue` (`looked-up`, `name`,
  `value`) and `RakuAST::WhereThunk` (`expression`).

Nothing was removed from any of the three tables. New unit tests pin that the
core-rule table stays sorted (it is binary-searched) and that the 2026.11
names are present.
