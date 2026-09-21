# List::Divvy's lazy prime pipeline now passes

`List::Divvy`'s upstream test suite now passes unchanged under mutsu. Its
`upto` routine combines a WhateverCode containing topic superscript power,
the first-class builtin `&is-prime`, and a lazy prime sequence passed through a
plain `@` parameter before taking `first :k`.

The parser now treats topic superscript spellings such as `.³` as powers of
`$_`, and `is-prime` is exposed through the `&` sigil as well as ordinary call
syntax. Binding a genuinely lazy `Seq`/`LazyList` to a plain `@` parameter now
keeps a shared lazy List view instead of draining it through
`PositionalBindFailover`. Lazy `first :k` pulls only until the matching index.

Regression coverage includes the superscript and `&is-prime` behavior plus a
bounded lazy-array-parameter test in `t/routines/signature/`. The full
`List::Divvy` test file passes 10/10 under both Rakudo and mutsu.
