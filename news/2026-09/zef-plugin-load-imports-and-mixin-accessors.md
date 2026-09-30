# zef plugins keep their imports, and a mixin keeps its wrapped class's accessors

A fresh-`$HOME` `mzef search` / `mzef install` died on zef 1.1.3 with
`Unknown function: lock-file-protect`, and an install copied every fetched
distribution into a directory named `Zef::Repository::LocalCache<NNNN>` in the
current directory (#10232). Three separate bugs were behind it:

- **`$*REPO.need` ran the compunit in the caller's scope.** The `FileSystem`
  (and `Installation`) repository's `need` parsed the module and ran its
  statements directly, instead of loading it as its own compilation unit the
  way `use`/`require` do. Its routines were attributed to the calling script's
  file, and when the calling routine had a `use` of its own, the subs the
  loaded module imported were rolled back with that routine's import scope.
  `need` now goes through `load_module` with the repository's resolved path
  (`Interpreter::load_module_from_path`), without importing into the caller.
- **A package-less provider's exports vanished after a registry rollback.** A
  module without a `unit` declarator registers `sub foo is export` as
  `GLOBAL::foo` — the same key the importer's alias uses, which a scope
  rollback (a parameter default, a block) deliberately does not reinstate.
  zef builds its plugin loaders inside `submethod TWEAK(:$!fetcher = ...)`
  defaults, so `Zef::Service::Shell::git` lost `uri` from `Zef::Utils::URI`.
  The importing module now keeps a private copy of such an alias.
- **A role mixin lost its wrapped class's `Any`-named accessors.**
  `Foo.new but role { ... }` answered `$.cache`, `$.list` and `$.elems` from
  the builtin `Any` methods (`.cache` is `(self,)`) instead of `Foo`'s
  attributes. The native fast path and the interpreter's by-name dispatch now
  defer to the wrapped instance's own methods and accessors.

With all three, `mzef install` into an empty `$HOME` completes, creates nothing
in the current directory and caches the distribution under `~/.zef/store`.
