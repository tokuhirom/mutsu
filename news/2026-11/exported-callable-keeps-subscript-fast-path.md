# An exported `&name` no longer turns off the element-read fast path

`@a[$i]` and `%h{$k}` on a plain Array or Hash are answered by a fast path
in front of the general subscript code (#8308). It declines when a user
`postcircumfix:<[ ]>` / `postcircumfix:<{ }>` could answer the read instead,
and one of its tests for that was "has any `sub EXPORT` handed out any
`&name` override". Loading a single module whose EXPORT hook returns an
ordinary callable (Cro's `&encode-percents` packages, for one) therefore
sent every element read in the whole program down the general path, about
2.7k more instructions per read.

Only an exported `postcircumfix` operator can answer an element read, so
`ModuleState` now records that one fact where an EXPORT hook installs the
name, and the fast path asks it.

With MCP::Server::Tool::Web loaded, `$known[0]` went from 3,800 to 1,070
instructions and `@a[1]` from 4,000 to 1,340 (callgrind slopes, profiling
build), and the module's html-to-text went from 4.186G to 4.032G
instructions per 0.05 of the fixture (-3.7%).
