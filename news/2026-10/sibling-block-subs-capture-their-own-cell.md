# A sub in a sibling block captures its block's variable in a cell

Two bare blocks that each declare `my $a` share one frame slot. A named sub
declared over the second block's `$a` used to get no cell for it, because a
cell in that slot could have been the first block's. Its writes reached the
block only through the return merge of the call.

Each block's sub now captures that block's own variable: the cell the
hoisted registration seeds for the block's declaration, or the binding the
block's `my` has already made. The blocks stay independent, and the write
no longer depends on the merge (ADR-12529 §3, slice 23).
