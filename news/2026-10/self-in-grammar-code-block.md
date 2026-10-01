# `self` in a grammar token's code block

A `{ ... }` block inside a grammar token or rule now sees `self` as the grammar
instance (cursor) of the rule invocation it belongs to, in both the compiled
regex engine and the tree walk. Previously it died with "'self' used where no
object is available".
