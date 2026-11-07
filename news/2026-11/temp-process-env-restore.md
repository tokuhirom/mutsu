# `temp %*ENV` restores the process-level hash

A `temp %*ENV` scope that stored a key published the temporized hash to the
process stash, but its restore did not publish the saved hash back (the
publish was gated on names starting with `*`, so `%*ENV` and `@*ARGS` missed
it). Afterwards every `%*ENV<K> = v` read back as `Nil`, and a loop or a sub
doing `temp %*ENV; %*ENV<K> := $v; run ...` lost the key from its second pass
on. Found through Code::Coverage's `t/01-basic.rakutest`, which now reaches
the same 9 passing assertions as the recorded accepted divergence (#15323).
