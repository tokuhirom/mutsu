# Template::HAML is not supported and is excluded from the ecosystem measurement

Maintainer decision (2026-11-08, ADR-16451 option (c)): mutsu does not create a
`<root>/.precomp` directory next to a `CompUnit::Repository::FileSystem` root; its precompilation
store stays in the per-user, content-keyed cache (ADR-11756). Template::HAML's
`t/0510-precomp-cross-process.rakutest` asserts that rakudo storage layout, and the maintainer
decided the distribution is not supported, so it is listed in `ecosystem/exclude.txt` and leaves
the work queue and bulk sweeps (`--only Template::HAML` still measures it). ADR-16451 is Accepted.
