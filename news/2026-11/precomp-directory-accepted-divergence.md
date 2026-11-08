# Template::HAML `.precomp` assertion recorded as an accepted divergence

Maintainer decision (2026-11-08, ADR-16451 option (c)): mutsu does not create a
`<root>/.precomp` directory next to a `CompUnit::Repository::FileSystem` root. Its precompilation
store stays in the per-user, content-keyed cache (ADR-11756). Template::HAML
`t/0510-precomp-cross-process.rakutest` test 1, which asserts that rakudo storage layout, is
recorded in `ecosystem/accepted-divergences.toml`, so it leaves the KPI denominator while still
failing loudly if it ever fails in a different way. ADR-16451 is now Accepted.
