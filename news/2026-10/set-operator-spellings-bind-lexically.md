# Imported set-operator candidates stay in their block, and set relations reduce

A block that imports user candidates of a set operator now keeps them in
that block for every spelling. That covers the Unicode spellings (`∪`, `∩`,
`⊎`, `⊍`, `∖`, `⊖`) and the set relations (`∈`, `⊆`, `⊂`, `(<=)`, `≡`, and
the rest). A routine written outside the block no longer reaches an imported
`∪` candidate. Inside the block, a call none of the candidates accepts is the
built-in operator's: `{ use M; set(1) ⊆ set(1, 2) }` used to die with
"Cannot resolve caller" and now answers `True`.

The set relations are now chain-associative reductions, as in rakudo.
`[(elem)] 1, (1, 2)`, `[∈]`, `[⊆]`, `[∉]`, `[(<=)]` and their triangle
forms died with "Unknown function" and now answer as rakudo does:
`[∈] 1, (1, 2), ((1, 2), 3)` is `False`, because it chains rather than folds
(#13794, ADR-8751).
