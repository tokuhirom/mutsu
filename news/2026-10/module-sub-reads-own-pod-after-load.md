# A module's routines read their own `$=pod` after the load

A sub declared in a `unit module` that read `$=pod` saw the importer's document
(an empty one for most programs) once the module had finished loading, because the
loader restores the importer's `=pod` env entry after the module body. The module's
document is now kept as a compunit lexical alongside its other file-scope lexicals, so
`sub late is export { $=pod.elems }` answers the module's own block count, as `raku`
does, while the importer's `$=pod` stays untouched.
