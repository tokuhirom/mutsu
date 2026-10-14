# PDF's `t/io.t` passes: parameterized types, spread containers, subclass `.isa`

PDF 0.6.17's `t/io.t` now passes all 37 tests, as under `raku`. Three
interpreter gaps stood in the way (#12907):

- **A parameterized type name skipped the compound-name scope guard.**
  Inside `class PDF::IO::Str`, `has Blob[uint8] $!ords` resolved to
  `PDF::IO::Blob[uint8]`, because the whole spelling was probed against the
  owner's package chain and the compound declared `class PDF::IO::Blob`
  matched. The base name is now resolved on its own (where the guard that
  keeps `PDF::IO` from acting as a scope applies) and the arguments are kept.
- **A built-in method refused container arguments from a spread capture.**
  `method subbuf(|c) { $.ords.subbuf(|c) }` called with `($!pos, n)` forwarded
  the scalar container of `$!pos`, and every native method declined it
  (`No such method 'subbuf' for invocant of type 'Blob[uint8]'`). The native
  dispatch now reads through a scalar container the way it already read through
  a native element reference; a user method still receives the container.
- **`.isa`, `.does` and `.can` on an instance of a `Str`/`Int` subclass**
  were answered by the payload, so `PDF::IO::Str.new(...).isa(PDF::IO::Str)`
  was False. They are now about the subclass.
