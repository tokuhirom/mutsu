# A named sub expression inside a routine is its own closure

`%subs<to-json> = sub to-json($obj, *%opt) { ... }` inside `sub EXPORT` of a module
that also has a file-level `to-json` evaluated to the *file-level* sub: the nested
declaration was compiled as "register, then read back by name", and the by-name read
found the compunit's sub. A plain `sub NAME` expression in a routine body now yields
the closure built at that point (the way `anon sub NAME` already did), so JSON::Native's
`use JSON::Native <!pretty>` wrapper works and t/05-drop-in.t passes.
