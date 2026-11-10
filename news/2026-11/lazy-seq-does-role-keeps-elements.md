# A lazy Seq with a role mixed in keeps its elements

`$lazy-seq does Role` and `$lazy-seq but Role` (a `.map`/`.grep` pipeline, a
`gather`) answered `.list`, `.elems`, `.join`, `.sum`, `.iterator`, ... from the
mixin wrapper, whose list view is the lazy list's still-empty cache, so they saw
no elements. An element-reading method that no composed role declares now runs
on the lazy `Seq` under the mixin. Qwiratry's streaming renderer, which mixes a
role onto a lazy Seq to detect `.list`, depends on it (#17333).
