# `nqp::split` honours grapheme boundaries

`nqp::split` cut strings at byte-level hits, so `nqp::split("\n", "a\r\n")` returned two
fields where MoarVM returns one (`\r\n` is a single grapheme). `Test.rakumod` escapes newlines in
a test description with that op, so a description ending in `\r\n` printed a stray `# ` line.
The op now calls `str_prim::split`, which only splits at separators that start and end on
grapheme boundaries. CSS::Grammar's `t/regex-tokens.t` output now equals rakudo's byte for byte.
