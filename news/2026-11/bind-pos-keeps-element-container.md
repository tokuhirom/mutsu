# `BIND-POS` / `BIND-KEY` keep the caller's element container

`@descent.BIND-POS($i, @array[$j])` bound a copy of the element, so a later write through the
bound slot died with "Cannot assign to an immutable value" (BinaryHeap's `!sift-down`). The value
argument of the built-in storage binders is raw in rakudo; the subscript-argument gate now answers
yes for it, so the element's own container is bound.
