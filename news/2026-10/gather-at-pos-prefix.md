# `.AT-POS` on a gather reads the Seq, not its inner coroutine list

`$g.AT-POS($i)` and `$g.EXISTS-POS($i)` on a plain gather used to answer from
the gather's inner coroutine list, a second identity for the same coroutine
(ADR-11678 invariant 1). They now pull into the Seq only the prefix the index
needs, exactly as `$g[$i]` does, and leave the Seq cached and readable. On an
infinite gather, `$g.AT-POS(2)` still runs the body only three times, and an
earlier index is answered from the cache without running it again.
