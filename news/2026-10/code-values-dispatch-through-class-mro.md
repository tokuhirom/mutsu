# Code values dispatch through their class's MRO

A code value's methods are now found through one lookup, `code_class::owners_of`, which decodes
the receiver to its built-in code class (`Sub`, `Method`, `Submethod`, `Block`, `Regex`,
`WhateverCode`) and names that class's owner chain as Rakudo declares it (ADR-14477). It replaces
`routine_owners_of`, the method-name gate of `code::answer`, `code_accepts_owners` and
`METHOD_OBJECT_ROWS`.

A `Block`, pointy block or `WhateverCode` no longer answers `Routine`'s `multi`, `dispatcher`,
`package`, ... (Rakudo dies with `No such method`), and a `Regex` now answers the `Routine` and
`Code` methods it inherits. `Code.cando` works on a `Sub`. `.^can` and the dispatch agree, pinned
by `t/oo/class/code-class-mro.t`.
