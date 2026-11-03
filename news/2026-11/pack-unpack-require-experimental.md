# `pack` / `unpack` need `use experimental :pack`

As on Rakudo, the `pack` / `unpack` routine forms are now an undeclared routine
unless the unit says `use experimental :pack`, and `Blob.unpack` raises
`X::Experimental` ("Use of the 'unpack' method is experimental; please 'use
experimental :pack;'") from code whose file lacks the pragma. The pragma is
recorded per source file when the `use` runs, so a precompiled module keeps it.
`is cached` is still unconditional: #16273. (#13346)
