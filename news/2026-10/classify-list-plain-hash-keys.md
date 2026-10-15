# `classify-list` into a plain hash stringifies its keys

`my %h; %h.classify-list({ $_ % 2 }, 1..6)` turned `%h` into an object hash
holding `Int` keys because every non-`Str` classifier result was recorded as a
key object. Rakudo keeps a plain `Hash` and stringifies the keys, so
`.keys`, `.keyof` and `.raku` now match. A standalone `classify` and an
object-hash `:into` target keep their key objects. Fixes #13572.
