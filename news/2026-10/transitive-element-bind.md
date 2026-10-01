# Binding to an element alias shares the element container

`my $q := @a[2]; my $n := $q` now binds `$n` to the very container `$q`
shares with `@a[2]`, as Rakudo does: a write through either name lands in the
array slot, and an element store is seen through both names. The same holds
for a sigilless alias (`my \r = @a[1]; my $m := r`) and for hash elements.

The `:=` bind recorded `$q`'s source as the compiler's per-site temporary
(`__mutsu_bind_index_ref_N`), and the alias-chain resolver walked into it, so
`my $n := $q` saw a source that was no variable of the frame, skipped the
shared-cell promotion and gave both names a detached copy. The resolver now
stops before such a temporary, since the last real variable on the chain
already holds the element's cell (#10530).

This was the cause of the `BinaryHeap` distribution's wrong heap order: its
sift-up loop rebinds the walker with `$node := parent`.
