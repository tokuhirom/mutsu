# `Lock.protect` reuses the analysis of its block per captured-name set

Every `protect` call whose block is a literal built a closure and then
re-derived which of the block's locals and global names the closure's captured
env holds: it cloned the name `String`s into three fresh `Arc<Vec<..>>`. That
set is a function of the compiled body and of which names the capture holds, so
`get_or_compile_protect_block_with_slots` now memoizes it by the body and two
membership bitmasks (bodies with at most 64 locals and 64 global names). A loop
that builds a closure per iteration reuses one result.

Part of #15530.
