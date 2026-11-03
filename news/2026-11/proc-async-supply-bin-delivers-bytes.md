# `Proc::Async.Supply(:bin)` delivers `Buf` chunks

`$proc.Supply(:bin).tap(...)` delivered the merged output as `Str`
(`["hi\n"]`; raku gives `Buf[uint8].new(104,105,10)`), which broke Shell::DSL's
`Pipeline.capture`. The `Supply` method ignored `:bin`. It now sets both
streams' modes to `bin`, so the reader threads emit `Buf`s into the merged
channel, and the live tap pump serves the merged Supply too when it was
requested as `:bin` (the text-mode merge is still replayed from the collected
text). Pinned by `t/concurrency/supply/proc-async-supply-bin-delivers-bytes.t`.

