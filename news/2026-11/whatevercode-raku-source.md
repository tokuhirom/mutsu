# A WhateverCode's `.raku` / `.gist` print the source it was written as

`my $c = *.not; say $c.raku` printed `WhateverCode.new`; rakudo (RakuAST) prints `*.not`, and
`* + 1`, `*.chars ≥ 3`, `*.contains: any <o a e>` likewise, verbatim. Testo's default test
description (`"{$got.raku} is {$exp.raku}"`) therefore differed for every `is $x, *.not`.

`Expr::WhateverCurry` now carries the source text the priming scope was written as (taken from
the span the expression parsers consumed, including the value of a `key => * + 1` pair). The
compiler hands it to the closure's `CompiledCode` (`whatever_source`, also in the precomp codec),
`build_closure` stores it in the closure's env, and `SubData::whatever_code_repr` is the one
renderer `.raku`, `.gist` and the list/array element renderers share. A WhateverCode whose scope
was primed somewhere the parser has no span for (a composed `o` operand, a call-chain split)
still prints `WhateverCode.new`.

Found by WhereList (`t/01-basic.t`). Test: `t/routines/closure/whatevercode-raku-source.t`;
`whatevercode-gist-raku.t` pinned the old spelling (it failed under `raku` too).
