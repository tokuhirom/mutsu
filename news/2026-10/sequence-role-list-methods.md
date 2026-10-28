# A `Sequence` class answers `pairs`/`kv`/`keys` from its iterator

A class that does `Sequence` and declares only `iterator` (Red's `ResultSeq`) now answers
Any's `self.list` methods (`pairs`, `kv`, `keys`, `elems`, `reverse`, ...) from its elements,
also when a role is mixed into an instance (`S.new but R`). Before, the whole object was one
element. Red t/38 (`unique`, `squish`, `rotor`, `pairs`, `kv`, `keys` on a result seq) depends on it.
