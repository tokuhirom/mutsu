# User postcircumfix candidates receive :k/:v/:kv/:p

A user-declared `multi sub postcircumfix:<{ }>` / `<[ ]>` on an object target now
receives the built-in slice adverbs as named arguments (`%m{ /\d/ }:k`), matching
Rakudo. Previously only non-built-in adverb names reached it, so `:k` and friends
bypassed the candidate. Found via Map::Match, whose `t/01-basic.rakutest` now
passes 18/18 under mutsu.
