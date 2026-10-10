# The list terminals are rows on the owners Rakudo declares them on

`List.head`, `Map.head`, `List.tail`, `Array.tail`, `List.sum` and `flat` on `Map` and `Range` are rows of the one method table (ADR-11276
§9.61, issue #12389). They reuse the handlers `Any` and `List` already had, so the answers are unchanged; `.^can` and the resolver now see
them where Rakudo declares them.
