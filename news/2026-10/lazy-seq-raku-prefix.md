# `.raku` of a lazy Seq reifies a 100-element prefix

`(1..*).map({ $_ * 2 }).raku` printed `(...)`. Rakudo reifies the first 100
elements, glues `...` to the last one when more remain and appends
`.lazy.Seq`. `Interpreter::lazy_seq_raku` (`src/runtime/lazy_seq_raku.rs`) now
renders that for a bare lazy Seq, reached from the VM's `CallMethod` /
`CallMethodMut` intercepts and the runtime method dispatch. The type-check
message of a failed assignment reuses it, cut by `short_repr_of_raku`. A lazy
`@` array keeps the `[...]` placeholder.
