The parser now recognizes parameter types re-exported by a `sub EXPORT` hook
that returns another module's export stash. It reads the stash reference from
the module's AST and uses the referenced module's scanned export tags, including
when a method supplies the hook's result. This keeps parse-time analysis free
of hook execution and leaves unexported types invalid.
