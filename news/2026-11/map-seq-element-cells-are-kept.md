# A `.map` Seq's element cells survive `[i]` and `.first`

`('a',).map(-> $k { %h{$k} })[0].VAR.^name` and `.first.VAR.^name` answered
`Int`; Rakudo says `Scalar` (the block returned the hash element's container).
The cells were already in the Seq (`.list[0]`, `$r[0] = 9` saw them); the `.VAR`
subscript path and the three `.first` paths decontainerized them. They now keep
the cell. `(1,2,3).grep(...)` over a list literal no longer hands out scratch
cells either (`Int`, as in Rakudo), so the two agree. A block returning its
argument or `$_` (`-> $k { $k }`) is the same feature one level up: #17094.
