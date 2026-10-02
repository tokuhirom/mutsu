# Spliced closure regexes run on the compiled regex engine

A Regex value that closed over its own scope, spliced into another pattern (a closure element of
`<@r>`), made the compiled regex engine (ADR-0135) decline the whole pattern. Its body now compiles
inline between two new ops that install and uninstall the closure's scope; each also leaves an entry
on the engine's undo trail, so backtracking into the body installs the scope again and backtracking
past it removes it. The body is matched lazily, exactly as the tree walk matches it.

Part of [#10255](https://github.com/tokuhirom/mutsu/issues/10255).
