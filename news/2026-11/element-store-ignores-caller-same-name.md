# An element store no longer picks up a caller's same-named variable

After an element store into an object (`$b[$i] += 1` on a `Buf`), the store
copied the variable's env binding back into the routine's local slot. That
lookup went past the routine's own frame into its callers' tiers. So when a
caller had a same-named variable bound in its env, such as a test's
`my $b = $table`, the sub's own `my Buf $b` was replaced by the caller's
value after the first store. The write-back now reads only the running
frame's own binding. This fixes Rakuish 0.121.1's `t/74-arrow-read.t` (#19186).
