# A subtest no longer makes a module permanently unusable

A `subtest` body is a block, so everything it declares — `class`, `role`,
`subset`, `sub`, `token` — is lexical to it and has to disappear when the block
ends. mutsu implements that by snapshotting the declaration registries before
running the body and restoring them afterwards.

The set of already-loaded modules was not part of that snapshot, and `use` is
lexical too. So a module first loaded *inside* a subtest lost every declaration
it had installed when the subtest ended, yet still counted as loaded. Every
later `use` of it then short-circuited on the "already loaded" early return in
`use_module_with_tags_inner()` and installed nothing, so its types stayed gone
for the rest of the file:

```raku
subtest 'A' => { use Foo; ok Foo::Thing.new.defined }   # passes
subtest 'B' => { use Foo; ok Foo::Thing.new.defined }   # X::InvalidType: Invalid typename 'Thing'
```

`loaded_modules` is now snapshotted and restored with the rest of the
declaration state, so the second `use` re-loads the module and re-installs its
declarations. Modules the mainline loaded before the subtest ran are unaffected
— they are in the snapshot, so they stay loaded.

While collecting the field into the snapshot, the three copies of the
save/restore sequence (the `subtest` normal path, its `plan skip-all` error
path, and `group-of`) were replaced by a single `SubtestDeclSnapshot` captured
by `snapshot_subtest_decls()` and applied by `restore_subtest_decls()`, so a
future addition cannot be forgotten in one of the three again.

This closes the `00-load` gap in the vendored Zef test suite: its second
subtest `use`s the `Zef::Service::*` plugins, all of which had already been
pulled in transitively by the first subtest's `use Zef`, so all twelve of them
failed with `Invalid typename 'Fetcher'` and friends. The Zef battery is now
10/10, and with it the release gate reaches its full baseline: **18/18 bundled
library test files pass**.

Pin: `t/subtest-module-reuse.t`.
