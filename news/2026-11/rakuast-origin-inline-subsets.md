# `.origin` of anonymous and inline subsets

An anonymous subset (`subset :: of Int`) and a subset used as a term
(`(subset T of Int)`, `1 ~~ (subset :: of Int where * > 0)`) now carry the
ranges rakudo 2026.11 gives them:
- the `Name` covers the name or the `::`;
- the `Trait::Of` covers `of TYPE`;
- the `Trait::Of`'s type covers the type name.

The parser now records these spans for the inline form too. The view places
them through the same path a named subset statement takes (#15780).
