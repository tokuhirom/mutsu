# `Mu.perl` is `self.raku`, resolved once

Rakudo declares `perl` once, on `Mu`, as a call to `self.raku`. mutsu treated the two names as
aliases at about a hundred places across the dispatch cascades, the VM's call paths and the
method rows, and the copies had drifted apart. The call entries now resolve a call to `perl` as
a call to `raku` unless a user-declared `perl` (a class's, a role's, an `augment`'s or a `.wrap`)
is in reach, and every alias below them is gone (ADR-11276 §9.152).

A user `method perl { ... callsame }` used to get `Nil` from its `callsame`. It now reaches
`Mu.perl`, which renders through the receiver's own `raku`. The scalar values (`Int`, `Num`,
`Rat`, `FatRat`, `Complex`, `Str`, `Bool`, `Version`) left the zero-argument rendering cascade
for their rows. `FatRat` gained `gist`/`raku` rows, and the zero-denominator renderings
(`<1/0>`, `FatRat.new(1, 0)`) moved into them.
