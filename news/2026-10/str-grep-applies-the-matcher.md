# `Str.grep(matcher)` applies the matcher to the string

The `Str` arm of `grep` answered the string itself for every matcher that was not a
regex-literal closure, so `"a".grep(Int)`, `"a".grep(/b/)` and `"a".grep({ $_ eq "x" })` all
kept the string. A `Str` is one item to `grep` like any other scalar; it now goes through the
same matcher and adverb handling (`:k`, `:kv`, `:p`) as every other single-item receiver.

Red's `Red::AST::transpose` walks `self.args.grep: Red::AST`; for a `Red::AST::Value` wrapping
`"*"` the string slipped through the filter and the walk died with "No such method
'transpose' for string '*'" (the `COUNT(*)` projection behind `ResultSeq.Bool`).

Refs #14094.
