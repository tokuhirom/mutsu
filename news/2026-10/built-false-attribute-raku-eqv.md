# A public `is built(False)` attribute is left out of the default `.raku` and `eqv`

Rakudo's default `.raku` lists only the attributes `.new` can set, so a public
attribute declared `is built(False)` is omitted, and the `eqv` of two instances
(which compares their `.raku`) ignores it. mutsu rendered and compared it.
`IRC::Log::Colabti`'s `merge` appended duplicate entries because every entry
holds a `$.log is built(:bind)` whose log (full of `is built(False)` state)
differed between the two logs; the merge now answers 4 as Rakudo does.
