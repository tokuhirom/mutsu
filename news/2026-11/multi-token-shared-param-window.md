# A `multi token` whose candidates share parameter names reads them at match time

The match-time parameter window of ADR-12917 used to apply only to a rule with exactly one
definition. It now applies to a `multi token` whose candidates all qualify and declare the same
positional names, so those rules keep one value-independent body (stable call edges, one parse per
body) instead of a source spliced per argument list (#14510).
