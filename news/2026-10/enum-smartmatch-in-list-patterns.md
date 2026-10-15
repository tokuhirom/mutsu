# Enum values smartmatch inside list patterns

List smartmatch now uses the same pure element comparison as top-level
smartmatch. A byte-buffer slice matched against a tuple of enum values therefore
passes its subset constraint and selects the constrained multi candidate, as it
does in Rakudo (#13545).
