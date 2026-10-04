# A sub called as a listop inside a regex code block takes a prefix argument

`"abc" ~~ /^ \w+ { say dec ~$/ }/` died with "Too few positionals" when `dec`
was a sub declared outside the regex: the code block parsed as `dec() ~ $/`.
Whether a bare word followed by `~`, `-` or `+` is a listop call depends on
whether the word names a declared sub, and the code block's parse knew none.

Both places a code block is parsed now know them. At parse time the block is
parsed from the enclosing scope stack (`parse_nested_block_fragment`), since it
is lexically inside it. At match time, when a pattern was not lowered from its
parse-time tree and only the code string is left, the parse is seeded with the
running program's routine names the way an EVAL's is. Closes #11616.
