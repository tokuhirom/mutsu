# Inner subs called from closures stay frame lexicals

ADR-0113's first slice bound a `my sub` as a frame lexical — never in the
routine registry, its call sites resolved at compile time — only when nothing
but bare calls in the routine body named it. A call from inside a closure of
that body (`@a.map({ helper($_) })`, `reduce -> $b, $i { helper($b) }`) still
disqualified the name, because the closure literal's AST is also stashed in the
chunk's `stmt_pool`, and several runtime paths compile that AST again instead
of running the closure's compiled chunk: the inline `map`/`grep` path, sequence
generators, and the carrier `eval_block_value` / `compile_block_raw` family
(`classify`, `deepmap`, ...). A chunk compiled there knew nothing of the
frame-lexical table, so its call would have found no registry entry.

Slice 2 lets those recompiles inherit the table instead:

- a closure literal's pool slot no longer disqualifies a name when every op that
  creates the closure carries its compiled chunk;
- `compile_loop_block_cached` (map/grep) and the sequence generator inherit from
  the closure's own chunk directly;
- the carrier compiles look the body up in
  `Interpreter::frame_lexical_closure_bodies`, which each closure-creating op
  fills when its chunk's new `CompiledCode::lexical_subtree` flag is set. The
  key is the address of the body's shared statement `Arc`, which the entry keeps
  alive so the address cannot be reused.

Across the vendored modules and zef, every "only called from a closure"
rejection is gone: 26 distinct inner subs are frame lexicals now (18 before),
among them the round helpers of `Digest::SHA1`, `Digest::SHA2` and
`Digest::RIPEMD`. `sha1` + `sha256` of a 9 KB string went from 4.2 s to 3.7 s
(release, wall clock, -12%).

What remains open under #9103 is an inner sub used as a value (`&name`), which
still takes the registry path; in the vendored corpus that is five inner subs,
none on a hot path. Pinned by `t/vm/scope/frame-lexical-inner-sub-closures.t`.
