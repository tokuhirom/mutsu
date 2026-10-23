# Rendering a deeply nested structure no longer scans its ancestors linearly

`.gist`, `.raku` and `.Str` keep the nodes they are inside so a cycle can be named
instead of recursed into. That stack was a `Vec` searched linearly at every node
(`gist.rs`, `raku_repr.rs`, `display.rs`), and `value_has_proxy` (which the
renderers run first, `builtins_lvalue.rs`) scanned its own `Vec<usize>` the same
way, so rendering a structure of depth `d` was O(d^2) (#12714).

`value::ancestor_stack::AncestorStack` replaces all of them: a stack that answers
"is this node an ancestor?" by a hash index once it is deeper than 32 and by a
scan of the few addresses below that, so a shallow render pays nothing for the
hash. `.gist` of 25,000 nested arrays: 3.17 s -> 0.24 s; of 50,000: 12.14 s ->
0.48 s (debug build, same box).

What is left is the copy of the child's text into each parent's: `[` + inner +
`]` copies the whole inner string at every level, O(d^2) bytes. At depth 200,000
that is 2.6 s for `.gist` and 5.6 s for `.raku` on a release build (the issue
measured 9.2 s and 32 s); rakudo itself needs 71 s for `.gist` at depth 25,000.
Removing it needs a renderer that writes into one shared buffer.
