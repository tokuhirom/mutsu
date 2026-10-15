# Capture's cascade arms answer Rakudo's values

The `Capture` block of the 0-arg cascade lost its `Int` (Rakudo's `Capture` has no
`.Int`) and `Bool` (already a row) arms, and its `List`/`Seq`/`flat` arms now read the
positional part like `Any.List` (`self.list`) instead of answering a one-element list
holding the capture: `\(1, 2, :a(3)).List` is `(1, 2)`. The names that have no row home
(`Hash`, `perl`, `is-lazy`, `WHAT`: they are `Any`/`Mu` answers a closed shape does not
reach) stay in the block until `Any`/`Mu` rows exist for them. Part of #11276.
