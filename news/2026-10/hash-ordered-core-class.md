# `Hash::Ordered`, rakudo's insertion-ordered Hash, is a real class

`class Tuple is Hash::Ordered` died with "cannot inherit from 'Hash::Ordered' because it is
unknown", and `my %h is Hash::Ordered` kept no insertion order (#15232, found via
Qwiratry's `TaOP`, which now loads). `Hash::Ordered` is declared as Raku source
(`src/runtime/run_prelude_hash_ordered.rs`) and injected into any compunit that names it, like
the `Iterator` role prelude: it keeps the key order beside the inherited `Hash` and answers
`keys`/`values`/`pairs`/`kv`/`antipairs`/`invert`/`iterator`/`map`/`grep`/`first`/`raku` from it.
