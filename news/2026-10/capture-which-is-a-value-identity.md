# `Capture.WHICH` is a value identity

`\(1, 2, :a(3)).WHICH` is now `Capture|(Int|1)(Int|2)|a(Int|3)`, a `ValueObjAt`, so two equal Captures are `===` as in Rakudo. It and
`List.Supply` are method-table rows (ADR-11276 §9.66).
