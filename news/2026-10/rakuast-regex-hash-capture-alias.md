RakuAST now round-trips mutsu's `%<name>=` and `%name=` regex hash capture
aliases through a structural named-capture node. Constructed hash aliases use
the same matcher semantics as source aliases. An unpaired `~` now raises its
regex diagnostic during structural validation, before either frontend tries
to convert the malformed pattern.
