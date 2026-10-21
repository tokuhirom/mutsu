# The native method path no longer re-decides lazy-pipe methods

`try_native_method_raw` (the VM's pure native entry) carried three blocks that special-cased a
lazy map/grep pipeline by method name: deferring `map`/`grep` to the interpreter, intercepting
`cache`, and retagging the laziness-preserving coercions (`Array`, `List`, `list`, `values`,
`cache`, `Seq`, `lazy`). They duplicated the lazy step (`method_table::lazy_pull`,
`lazy_stream`, ADR-13891), which every VM method lane and the interpreter entry already run
ahead of the native path, so a lazy receiver never reached them.

The blocks are deleted (58 lines, six quoted method-name arms). The `X::Cannot::Lazy` guard
for eager list operations stays: it also serves ranges, which the lazy step does not own.

Part of #11276 (slice 5, deleting the name-matching checks of the native entry).
