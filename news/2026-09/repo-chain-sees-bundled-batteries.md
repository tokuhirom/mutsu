# `$*REPO` now sees the bundled batteries

`use Test` has always worked, but `$*REPO.resolve(CompUnit::DependencySpecification.new(:short-name<Test>))`
answered Nil ([#9071](https://github.com/tokuhirom/mutsu/issues/9071)). mutsu loads its bundled
batteries (`modules/<Dist>/lib`, including the `Rakudo-Core` distribution that carries `Test` and
`Pod::To::Text`) through `resolve_module_path`'s lowest-priority fallback, and that fallback sat
*outside* the `CompUnit::Repository` chain. The repository API (`resolve`, `need`, `repo-chain`)
walks only the chain, so it could not find anything `use` found there. Since #9004 made `resolve`
follow `next-repo`, this was the only reason a lookup of a core or bundled module came back empty.

## What changed

- **Each bundled distribution is now a link at the tail of `$*REPO`'s chain.** Every interpreter
  that registers the default site repository also appends one `CompUnit::Repository::FileSystem`
  per bundled distribution after it, in the same order `resolve_module_path` searches them. That
  models each battery as what it is on disk: a source distribution with its `META6.json` one level
  above its `lib/`. `resolve`, `candidates` and `need` on those links reuse the existing FileSystem
  code, so a resolved `CompUnit`'s `.distribution.meta` is the battery's real `META6.json`.
- **Precedence is unchanged.** The new links come after every `use lib`, `-I`, `MUTSULIB` and
  site-repository link, which is exactly where the fallback searches bundled modules. `use` and the
  repository API therefore agree on which copy wins.
- **They are FileSystem links, not Installation ones, on purpose.** zef's `list-installed` and
  `is-installed` read only `CompUnit::Repository::Installation` repositories. A bundled battery
  still reads as *not installed*, so `mzef install` can still put a newer copy in the site
  repository that shadows it (BATTERIES.md §6).
- **`CompUnit::RepositoryRegistry.repository-for-name('core')`** is still an Installation
  repository, as it is in Rakudo, and `.candidates('CORE')` is still empty. But the `Rakudo-Core`
  battery is now chained behind it as its `next-repo`, so `core.resolve(Test)` finds `Test` the way
  Rakudo's does, and `core.resolve(JSON::Fast)` still does not. This relies on
  `CompUnit::Repository::Installation.new` now accepting `:next-repo`, which Rakudo's also accepts.

Pinned by `t/modules/compunit/repo-resolve-bundled.t`.

## Not in this change

The issue also noted that `CompUnit::Repository::FileSystem.load(IO::Path)` returns Nil, where
Rakudo returns a loaded `CompUnit` and records it in `.loaded`. That is a separate gap and is not
fixed here. It is tracked as [#9079](https://github.com/tokuhirom/mutsu/issues/9079) (the method is missing outright, not returning Nil).
