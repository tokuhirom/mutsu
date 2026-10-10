# A named `is copy` pointy parameter on `with` no longer writes back

`with $_<V> -> $v is copy { }` inside a `for $table<R> { ... }` replaced the source
element with the enclosing topic on exit, corrupting the data table (`R<V>` became the
whole `R` hash). The `is copy` pointy parameter records no param name for the `Given`
writeback, so the writeback fell back to `$_`, which had already been restored to the
outer topic. A detached `is copy` pointy parameter now skips the writeback entirely.

Found through `Font::AFM`'s `t/font-metrics-times.t` (`.kern(..., $pointsize)` on the
second call read a corrupted kern table); the file now passes under mutsu.
Regression test: `t/control/with-pointy-is-copy-no-writeback.t`.
