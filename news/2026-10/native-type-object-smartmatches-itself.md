# A native type object smartmatches its own native type

`num64 ~~ num64` (and `int`, `uint8`, `str`, ...) was False: the native-type
smartmatch only answered for a *variable declared* with that native type, so a
type object, or a variable merely holding one, never matched. The match now
accepts a type object of the same native type. `LLM::RetrievalAugmentedGeneration`'s
`VectorDatabase.new` normalises its `num-type` with
`when $_ ~~ num32 || $_ ~~ num64` and died with "The argument $num-type is
expected to be one of the strings ..." for the default `num64`.
