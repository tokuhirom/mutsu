RakuAST now round-trips `has Int:_ $.x` using the measured simple `Int` type
node. The smiley adds no declaration type refinement, so lowering keeps using
the existing typed-attribute path. The frontend ratchet increased to 7,944 of
7,988 files.
