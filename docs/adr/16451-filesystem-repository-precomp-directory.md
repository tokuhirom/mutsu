# ADR-16451: Whether a `CompUnit::Repository::FileSystem` root gets a `.precomp` directory

- **Status**: Accepted (maintainer decision 2026-11-08: option (c))
- **Date**: 2026-11-08
- **Deciders**: ecosystem inventory worker, for the maintainer's review
- **Issue**: [#16451](https://github.com/tokuhirom/mutsu/issues/16451) (Template::HAML
  `t/0510-precomp-cross-process.rakutest`, test 1 of 6)
- **Related**: [ADR-11756](11756-compiled-bytecode-precompilation.md),
  [ADR-12026](12026-precomp-hit-loads-from-facts-and-decodes-lazily.md),
  [ADR-0130](0130-ecosystem-accepted-divergences.md), [docs/security.md](../security.md)

## 1. Context

After `require`-ing a module through a `CompUnit::Repository::FileSystem` rooted at a directory,
rakudo leaves `<root>/.precomp/` behind; the test asserts it exists and is a directory. mutsu's
precompilation store is a per-user cache keyed by content (ADR-11756, `src/precomp.rs`
`cache_dir()`), never next to the source. The other five tests of that file pass.

Nothing in `raku-doc` or roast specifies where the store lives or that a `.precomp` directory
exists: it is a rakudo storage layout.

## 2. Options

**(a) Create `<root>/.precomp/` and keep mutsu's cache entries (or a mirror) there.** Matches
rakudo's observable layout. Costs: the runtime then writes into a user's source tree on its own
(permissions, read-only checkouts, VCS noise), which docs/security.md only tolerates for paths
the user named *and* opened with `O_NOFOLLOW`/close-on-exec discipline; and it splits the cache
(ADR-11756's privacy and eviction story assumes one per-user store).

**(b) Create only an empty `.precomp/` marker directory.** Passes the test, stores nothing:
a hardcoded artefact to satisfy one assertion. Rejected by the "no stubs" rule.

**(c) Do not copy the layout; record the file as an accepted divergence.** One entry in
`ecosystem/accepted-divergences.toml` (shape: Template::HAML `t/0510`, test 1), exactly the
ADR-0130 case (a rakudo implementation artefact the language does not require).

## 3. Decision

Option **(c)** (maintainer decision, 2026-11-08). The store location is an implementation detail; mutsu's per-user cache is the
decision ADR-11756 already took, and (a) weakens the "no runtime-written files in user trees"
posture for one assertion. Revisit only if a second distribution is found that reads `.precomp`
as data rather than as an existence check.

## 4. Consequences

Template::HAML `t/0510` leaves the KPI denominator as `accepted` (it still fails if it starts
failing differently). #16451 closes with the entry; no code change.
