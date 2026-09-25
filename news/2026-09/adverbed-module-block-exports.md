# Exports of an adverbed `module Foo:auth<...> { }` block reach the importer's parse

`PatternMatching` declares its module as
`module PatternMatching:auth(...):ver(...) { ... }` and exports two operators
from it; `t/01-pattern-matching.rakutest` died at its first use of `┇` with this
#7988 cluster's generic `Confused. expected statement: ...` message.

Two gaps in the importer's static export scan (`module_exports.rs`) stacked:

- A block-form declarator carrying adverbs is wrapped by the parser in a
  `SyntheticBlock` together with its metadata statements. The export walker
  (`collect_exported_subs_in`) did not descend into that wrapper (nor into the
  bare `Block` a trait-carrying declarator gets), so **every** `is export`
  routine of an adverbed module block was invisible to the importer. A word
  operator still parsed through the speculative infix-word matcher, which hid
  the gap; a symbol operator such as `⊕` did not. The exported-constant
  collector had the same blind spot and walks the wrapper now too (the type-name
  collector already did).
- An operator exported as a code variable,
  `our &infix:<┇> is export = &[match_pattern];`, was not collected at all. An
  `is export` `VarDecl` whose name carries the `&` sigil is now recorded under
  the routine name, like `sub name is export`.

Pinned by `t/modules/import-export/adverbed-block-module-exports.t` (fixture
`t/lib/AdverbedBlockModuleOps.rakumod`). PatternMatching's test file now parses
and runs; 14 of its 20 assertions still fail on a separate precedence gap — a
user infix `is equiv<Z>` does not get list-infix precedence — filed as #9405.
