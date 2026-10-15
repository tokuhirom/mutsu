# Capture's remaining cascade arms are rows

`Capture` no longer has a name-matching arm block in the 0-arg cascade. `Bool` is a
row, and `Hash`, `List`, `Seq`, `flat` and `perl` (answers Rakudo reaches through
`Any`/`Mu`, which a closed shape does not) are rows of the `Capture` owner. Behaviour
moves toward Rakudo: `\(1, 2, :a(3)).List` is `(1, 2)` and `.flat` is `(1, 2).Seq`
(both used to hold the capture itself), `.Hash` is `{a => 3}`, `.perl` is the `.raku`
text, and `.Int` is no longer answered (Rakudo has none). Part of #11276.
