# A `sub EXPORT` hook's type alias is a type name to the importer

A type that a module's `sub EXPORT` hook hands out under a literal key
(`Map.new('MyAlias' => Int)`) can now be named in a signature of the
importing code. `{ use M; sub f(MyAlias $p) { ... } }` died before the
program ran, with `Invalid typename 'MyAlias' in parameter declaration.`
(#14140).

The parse-time module scan collects the hook's uppercase literal keys whose
value is a bare type name (`export_hook::collect_export_hook_type_keys`). It
adds them to the module's type names for the importer's parse, and records
them for the run time's pre-pass type checks (`scanned_hook_type_keys`,
read by `collect_use_declared_type_names`). At run time the name still
resolves through the import's env alias.
