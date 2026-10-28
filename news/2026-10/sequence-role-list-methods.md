# A `Sequence` class answers pairs/kv/keys/flat/any/[*-1] from its iterator

A class that does `Sequence` and declares only `iterator` (Red's `Red::ResultSeq`, also under a
`but` mixin) now answers `pairs`, `kv`, `keys`, `values`, `flat`, `reduce`, `unique`, `squish`,
`rotor`, `batch`, `any`/`all`/`one`/`none`, `pick`/`roll`, `AT-POS` and `Numeric` from its
elements, and `$seq[*-1]` resolves against its `elems`. Before, the whole object was one element.
Red t/38 (parity of a result seq with `Seq`) depends on it.
