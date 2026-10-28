# `%@a` / `@%h`, and a module naming its own package as a term

`%@a` (hash-contextualize an array) and `@%h` (list-contextualize a hash) now parse in
expression position. A `module`/`package` declared in a module file can be named as a bare
term (`ann::cont`) from a sub of that file; it was mistaken for a call of `&cont` in `ann`.
Both were found working the `annotations` distribution; its remaining gaps are #15258 and #15259.
