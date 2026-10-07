# `WHICH` on the collections and `gist`/`raku` on the quant hashes are method-table rows

`Array`, `Hash`, `Pair`, `Range`, `Set`, `Bag` and `Mix` register `WHICH`, the six quant hashes register `gist` and
`raku`, and `Range` registers `raku` (ADR-11276 §9.31). The cascade's quant-hash and `Range` rendering arms now call the
same handlers, so each of these has one implementation.
