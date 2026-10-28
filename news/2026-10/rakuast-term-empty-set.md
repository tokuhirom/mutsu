# `∅` is `RakuAST::Term::EmptySet`

The parser folds `∅` to a literal empty Set, and the RakuAST converter refused every literal Set ("literal Set(...)"), so any unit containing `∅` failed the round-trip. Physics::Unit's `has MixHash $.dmix is rw = ∅.MixHash` kept the whole module from loading. `∅` now converts to `RakuAST::Term::EmptySet` (rakudo's node, measured on 2026.09), is constructible and lowers back to the empty Set.
