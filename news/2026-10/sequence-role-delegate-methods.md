# One Sequence-role delegate answers a `Sequence` class's list methods

The parallel "own iteration" handling of `does Sequence` classes (added a few hours earlier for
Red's `ResultSeq`) is gone: the existing `vm_sequence_role_delegate`, which already drains the
iterator of a `does Sequence` class (also under a `but` mixin), now covers `pairs`, `kv`, `keys`,
`values`, `flat`, `reduce`, `unique`, `squish`, `rotor`, `batch`, `any`/`all`/`one`/`none`,
`pick`/`roll`, `AT-POS` and `Numeric`, and `$seq[*-1]` resolves against its `elems`.
Red t/38 now fails only on `sort(*)` and `categorize`.
