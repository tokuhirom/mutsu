# Rakudo-Type-Introspection is not supported and is excluded from the ecosystem measurement

Maintainer decision (2026-11-08), superseding the 2026-11-03 decision to implement it: like the
other rakudo-specific distributions, Rakudo-Type-Introspection reads rakudo's own undocumented
internals (the version-gated `CORE::v6c|v6d|v6e` namespaces, `CORE::v6c::PseudoStash`,
`.^is-implementation-detail`), so mutsu does not support it. It is listed in
`ecosystem/exclude.txt` (out of the work queue and bulk sweeps; `--only Rakudo-Type-Introspection`
still measures it). #15616 is closed accordingly.
