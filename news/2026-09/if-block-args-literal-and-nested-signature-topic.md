# A `"%_"` literal no longer corrupts the stack; nested-signature `$_` keeps a hash

Two parser/compiler bugs, found by running LLM::Graph 0.1.1's own suite.

**An `if` block that spelled `@_`/`%_` in a string ate the caller's stack slot.**
A bare `if` block binds its condition as `@_` when its body uses `@_`/`%_`. The
probe, `body_uses_legacy_args`, scanned the body's `Debug` text for `"@_"` and
`"%_"`. That matched string literals such as the word list in
`when $name ∈ <$_ @_ %_>`, but not real reads, which format as `ArrayVar("_")`.
When it fired, the no-`else` statement form also emitted the `Pop` for the
leftover duplicated condition where the taken branch falls through into it.
So `sub f { if $c { my $h = '%_' }; 'r' }; say $_ => f()` printed `r => Nil`:
the extra pop consumed the pair key the caller had already pushed. An
`if $c { @_ = 7 }` in a sub broke the same way. The probe now counts only a
sigiled `@_`/`%_` target, and the false-branch `Pop` is jumped over on the taken
path. Binding real `@_` reads, which needs a branch-scoped `@_`, is #9979.

**`{ a => sub ($_ = Whatever) { … } }` was a Block.** The lexical
hash-composer-vs-block scan treated a `$_` inside a nested `sub (…)`, `-> …` or
`<-> …` signature as a reference to the outer block's topic. That `$_` belongs
to the inner closure, and rakudo keeps the outer braces a Hash. The scan now
skips a nested signature.

LLM::Graph goes from 1/4 to 3/4 baseline files at parity: `t/02-evaluation`
and `t/03-evaluation-async` now pass. `t/04-spec-synonyms` needs #9978: a
multi-parameter `for … -> $a, $b` overwrites the outer `$_` with the batch.
