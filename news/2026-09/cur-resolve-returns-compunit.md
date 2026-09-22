# `CompUnit::Repository.resolve` returns a real `CompUnit`

`CompUnit::Repository::FileSystem` had no `resolve` method at all, and
`CompUnit::Repository::Installation.resolve` was a stub answering a bare `Bool`
("does any candidate match"). Rakudo's `resolve` takes the best candidate for a
`CompUnit::DependencySpecification` and describes it as a `CompUnit` whose
`.repo` is the resolving repository and whose `.repo-id` is a stable
per-compunit hash, delegating to `next-repo` when the repository itself has no
match. `Identity::Utils`' `compunit` / `bytecode-io` depends on exactly that
(`with $repo.resolve($spec) { my $repo := .repo; my $repo-id := .repo-id }`),
which left `Code::Coverable` and `Code::Coverage` dying with
`No such method 'resolve' for invocant of type 'CompUnit::Repository::FileSystem'`
(issue #9004).

Both repository kinds now share one implementation
(`src/runtime/methods_distribution_cur_compunit.rs`):

- the head of the existing candidate enumeration (`cur_fs_candidates` /
  `cur_inst_candidates`) becomes the CompUnit's `.distribution`;
- `.repo-id` is an uppercase SHA-1 of the distribution identity plus the
  short-name, as Rakudo's `CompUnit::PrecompilationId.new-from-string($dist.id ~ $name)`
  does -- the `dist-id` for an installed distribution, the canonical prefix for
  a FileSystem repository's implicit one;
- the CompUnit is unloaded: `.handle` is the `CompUnit::Handle` type object and
  `.precompiled` is False; an Installation CompUnit reports the installed
  `version`/`auth`/`api`, a FileSystem one leaves them as type objects (both
  measured against Rakudo 2026.07);
- no match falls through to `next-repo.resolve`, else `Nil`.

Supporting fixes found on the way:

- `CompUnit::Repository::FileSystem.new(:next-repo(...))` dropped the
  `next-repo` argument entirely; the first construction for a prefix now keeps
  it (Rakudo caches the instance per prefix the same way).
- `CompUnit` and `CompUnit::Handle` were not declared type names, so
  `$x ~~ CompUnit` died with "Undeclared name". Both are now builtin types.
- `~$compunit` stringifies to the short-name.
- A depspec's `version-matcher` / `api-matcher` was compared to the meta's
  version as a *string*, so zef's own probe
  `$*REPO.resolve(... short-name => 'TAP', version-matcher => '0.3.5+')`
  could never match. It is now smartmatched as a `Version`, like Rakudo.

Found by walking `raku-doc/doc/Type/CompUnit/Repository/*.rakudoc` against
mutsu with Rakudo as the oracle, and fixed in the same change:

- `need` now returns the same identity `resolve` reports: its CompUnit carries
  `.repo`, `.repo-id` and `.distribution` (for both repository kinds).
- `CompUnit::Repository::FileSystem` gained `loaded` (the units `need` loaded),
  `id` and `path-spec`.
- `Installation.need` picked the "highest" candidate by *string* order
  (`1.9` > `1.10`); `need` and `resolve` now share one Version-ordered pick.
- `CompUnit.new(:short-name, :repo, :repo-id, ...)` works.

Two gaps remain, recorded in #9071: `$*REPO.resolve` does not see bundled
modules such as `Test` (mutsu serves them outside the repository chain), and
`FileSystem.load(IO::Path)` is not implemented.

The `CompUnit` accessor methods moved out of the oversized
`methods_instance_ops.rs` into the new module. `Code::Coverable`'s
`t/01-basic.rakutest` now gets past `resolve`; its next failure is an
unrelated RakuAST gap (`.AST` of a `use` statement).

Pinned by `t/modules/compunit/cur-resolve-compunit.t`.
