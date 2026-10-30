# `MY::` in a block that `use`s a module no longer lists the file-level packages

A nested block's pad holds what its own declarations and `use`s put there. The pseudo-stash
builder also added every `Package` value of the runtime environment, so a file-level
`use Test` showed up as `Test` in `MY::` inside `{ use SomeModule; ... }`, where `raku`
lists only the block's import. Only a compunit's root (or a `LEXICAL::` view) now reads
those environment packages (#15532).
