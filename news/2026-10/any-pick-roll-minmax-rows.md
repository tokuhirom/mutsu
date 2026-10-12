# `Any.pick`, `Any.roll` and `Any.minmax` reach every receiver from one row

The name-matching `pick`, `roll`, `pickpairs` and `minmax` arms of the method cascades are gone (ADR-11276 §9.83).
`Any.pick` and `Any.roll` are method-table rows that a type object may call, and every receiver without a candidate
of its own (a `Capture`, a `Blob`, a `Seq`, a `Slip`, a code object, a user-class instance, a type object) reaches
`Any`'s rows, which read it as its list. `Mu` is not an `Any`, so `Mu.pick`, `Mu.roll` and `Mu.flat` now raise
`X::Method::NotFound` as in Rakudo instead of answering `Mu`. The sub forms `pick($n, @list)` and `roll($n, @list)`
call the same implementation as the methods. Eight more arms leave the pure arity cascades (316 to 308).
