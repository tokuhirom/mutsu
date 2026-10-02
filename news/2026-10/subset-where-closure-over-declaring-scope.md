# A subset `where` block is a closure over its declaring scope

`my $n = 0; subset C where { $n++; True }; my C $s = 1; say $n` printed `0`
(rakudo: `1`), and so did the anonymous subset `my $x where { $m++; True }`
desugars to (#10868). The type check ran the predicate's body against the
*checking* frame's env, so a store to an outer scalar landed in that env copy
and never reached the declaring frame's local slot. Reads were only right by
accident too: they saw whatever the checking frame held under the name.

A predicate written as a code literal (a block, a pointy block or a
WhateverCode) that refers to an outer lexical is now built as a real closure
at the declaration site. The compiler emits it in an escaping position, so the
lexicals it writes get shared cells. `RegisterSubset` takes it from the stack,
and the subset definition keeps it (traced as a GC root). The type check calls
that closure. A predicate with no free lexical (`where * > 0`,
`where { $_ %% 2 }`) depends only on its argument and keeps the inline check.

Making the writes visible exposed that a subset-typed method parameter runs
its predicate twice on the first call; that is #10935.
