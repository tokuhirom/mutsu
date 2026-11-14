# `handles` keeps working after a bind and inside another delegation

Two bugs in the mutating dispatcher's `handles` forwarding, both hit by
Font::FreeType's `Face` (PDF::Font::Loader::CSS):

- Once a method of the class bound the attribute (`my $r := $!raw`), the
  attribute held a Scalar cell. A delegated call (`$face.num-glyphs`
  through `has FT_Face $.raw handles <num-glyphs ...>`) was then sent to
  the cell itself, which names no class, and died with "No such method".
  The call now goes to the object in the cell, as the non-mutating path
  already did, and an updated delegate is stored back into the cell, so
  every alias of it stays live.
- The delegate was parked under one fixed env name and removed
  afterwards. A delegation inside the block of another (a delegated
  `self.n` inside `$.protect: { ... }`, where `protect` is
  `handles<protect>` on a Lock) removed it from under the outer one. The
  outer write-back then stored Nil into the Lock attribute, and the next
  `$.protect` found no Lock. The binding is now saved and restored, also
  when the call fails.

Tests: `t/oo/attribute/handles-delegate-bound-attribute.t`,
`t/oo/trait/handles-nested-delegation-keeps-delegate.t`.
