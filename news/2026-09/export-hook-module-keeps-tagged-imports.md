# A module with `sub EXPORT` still imports its `is export(:tag)` subs

A `use M :tag` of a module that also has a `sub EXPORT` hook lost the tagged
subs in three shapes (#9389):

- the import was in a block (`{ use M :u; u() }`);
- the tagged sub was declared inside the module's class body (the
  `Terminal::ANSI::OO :t` shape);
- a later `{ use M; }` re-`use` hid an earlier file-scope `use M :t`.

In all three cases, `u()` / `t()` died with "Unknown function".

The cause was one rule in `use_module_with_tags_inner`: for a module with a
hook, it dropped every tag except `:ALL`, to keep a positional word list such
as `use JSON::Fast <immutable !pretty>` from being read as tags. rakudo
separates the two by syntax. Positional arguments go to `EXPORT`, and
colonpairs select export tags. An undeclared tag is still
`X::Import::NoSuchTag`. mutsu's parser already made the same split. Only the
compiler, for `JSON::Fast`/`JSON::Tiny`, also copied the word list into the
tags. No runtime code reads that copy any more; it is left over from the
native JSON provider. So the compiler no longer copies it, and a hook
module's colonpair tags are imported like any other module's.

A fourth shape came up in the issue thread. A hook module was first loaded
in a block, and another module then imported it. That module's own
routines could not see the term the re-run hook installed:
`ExportHookTermUser.new.go` died with "Unknown function: t". A symbol
installed by `sub EXPORT` is now recorded in the loading module's
`module_imported_names`, as `import_module` already does for tag imports, so
it becomes part of that module's scope. `export_installed_term` consults
that scope (`module_imported_lexical`) before the caller's env.

`t/modules/import-export/export-hook-term-shadows-tagged-sub.t` had been split
in two to avoid these bugs. It is back to one file, with `t()` restored and
the new shapes added.
