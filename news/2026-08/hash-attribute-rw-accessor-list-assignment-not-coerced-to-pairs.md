# Hash attribute rw accessors coerce list assignments to pairs

Whole-value assignment through a public `%` attribute accessor now uses the
attribute's declared hash context. Assigning an alternating list such as
`$object.values = <Z Y X W>` therefore stores a `Hash` containing `Z => Y` and
`X => W`, matching direct assignment to a `%` variable.

Previously, an attribute initialized from a non-literal list could retain that
list as its internal pre-assignment value even though accessor reads correctly
presented it as a Hash. The write path inferred assignment context from that
internal value's runtime shape and consequently stored the replacement list
without Hash coercion. The accessor assignment path now normalizes the right-hand
side from the attribute declaration's `%` sigil before type checks and storage.
