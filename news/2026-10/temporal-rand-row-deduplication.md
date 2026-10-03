# Deduplicate temporal rand method rows

Keep one native method catalog row each for `Duration.rand` and `Instant.rand`.
This preserves their introspection metadata and lets the catalog's unique-key
check pass.
