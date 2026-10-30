# Unicode `^…` / `^…^` sequence operators are pinned by a test

`1 ^… 5` and `1 ^…^ 5` already behave like their ASCII spellings `^...` / `^...^` on
`main` (the sequence-operator scanner was extended in #15206), but nothing pinned them.
A focused test now does, closing #14913.
