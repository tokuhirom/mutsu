# `unique(:with(&[===]))` takes the hashed path; core infix comparators apply directly

`unique`, `repeated` and `squish` under `:with` compare each element with every
key kept so far. With `:with(&[===])` that is the default comparison, which
rakudo answers on its hashed path; mutsu ran the quadratic scan and called the
operator routine per pair, so ParaSeq's `t/20-unique` (10000 elements) took
minutes. The core `===` is now dropped to the default rule, a built-in infix
comparator such as `&[==]` is applied directly instead of being called as a
routine, and the scan no longer copies the kept keys for every element: 2000
elements under `:with(&[==])` went from 1.8 s to 0.16 s (rakudo 0.09 s).
