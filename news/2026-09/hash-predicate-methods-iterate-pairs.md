# Hash predicate methods iterate pairs through scalar holders

`Hash.grep`, `Hash.first`, and `Hash.classify` now iterate a Hash's key-value
pairs even when the Hash is held in a scalar or reached through a Hash element.
Predicate blocks therefore receive the same `Pair` values as they do for a
direct `%`-sigiled Hash.

Pinned by `t/collections/hash/hash-predicate-iteration.t`.

Closes #8211.
