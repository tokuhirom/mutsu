# A scalar parameter mutated only by `:delete` / `++` is captured by the closure

`sub w($x) { (1,).map({ $x{4}:delete }) }; w($h)` left `$h` untouched when the caller
sank the returned Seq. The closure's only use of `$x` was a `DeleteIndexNamed` op, which
the free-variable scan counted for `@`/`%` containers but not for a scalar holding one,
so the block ran after the frame was gone against a stale copy. `DeleteIndexNamed` and
the element `++`/`--` ops are now in the by-name read set like the element-assign ops.
With the object-hash delete fix, this makes App::RaCoCo's `t/11-report-data-part.rakutest`
pass.
