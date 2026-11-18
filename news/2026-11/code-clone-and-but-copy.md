# `nqp::clone` and `but` copy code objects

`nqp::clone` of a code object returned the object itself. Rakudo's clone is a
distinct object that shares the original's body (`$!do`), so it also shares
the `state` variables and the `FIRST` flag, while a bind on the copy stays the
copy's. ParaSeq's `BlockMapper` relies on that: it clones the user's block for
every batch and binds a `$!phasers` table on each copy, giving only the first
batch the `FIRST` phaser. In mutsu every batch bound its table on the same
object, so a later batch's table (without `FIRST`) replaced the first batch's
before it ran, and a hypered `.map` never fired the block's `FIRST`. Now the
copy is a new code object in the original's state scope, and a code object
with roles mixed in gets its own attribute slots.

`$code but R` mixed the role into the original code object rather than into a
clone, so the mixin shared the original's `state` variables and its `FIRST`
flag. Rakudo's `but` is `.clone.^mixin`, so the mixin now starts from a fresh
closure clone, as `$code.clone` does.

ParaSeq's phaser map (`hyperize(16, 3).map({ FIRST ...; ... })`) now fires
`FIRST` exactly once, as the module intends. It had fired 0 times on most
runs. The local rakudo still fires it twice; ParaSeq marks that test TODO for
rakudo before 2024.06.
