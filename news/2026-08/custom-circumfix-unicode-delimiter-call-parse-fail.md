# A custom circumfix's closing delimiter is no longer eaten as a speculative infix

`sub circumfix:<α ω>($a) { say $a * 2 }; α 5 ω;` parsed in rakudo (`10`) but
failed in mutsu with `Confused. expected statement: ... expected expression after
infix operator`.

## Root cause

Not the Unicode-ness of the delimiter, as the ticket assumed. The same failure
reproduces with pure ASCII lowercase delimiters — `sub circumfix:<foo bar>(...)`
called as `foo 5 bar;` failed identically, while the uppercase
`sub circumfix:<FOO BAR>` / `FOO 5 BAR` worked.

`declared_circumfix_op` matches the opener, then parses the argument with
`expression()`. That descends into the custom-infix-word matcher, which is
deliberately permissive — it takes *any* non-reserved word as a possible infix,
since an infix can be installed at runtime with nothing for the parser to
consult. So `ω` (and `bar`) were claimed as an infix operator after the `5`,
leaving the circumfix unclosed. Uppercase closers only survived by accident:
`is_reserved_infix_word` rejects anything starting with an ASCII uppercase
letter, so `BAR` was never a candidate.

The ticket also claimed the multi-character Unicode form `circumfix:<αX Xω>`
should work; it does not — rakudo rejects `αX 5 Xω;` with "Bogus statement" too,
so mutsu rejecting it is correct.

## Fix

`is_circumfix_close_delimiter_word` (`src/parser/stmt/simple/user_ops.rs`) answers
whether a whole identifier is exactly the closing delimiter of an in-scope
`circumfix:<open close>` / `postcircumfix:<open close>`, and
`parse_custom_infix_word` now refuses such a word. The pre-existing
`is_circumfix_close_delimiter` could not be reused for this: it is unanchored, so
with `circumfix:<foo bar>` in scope it also answers `true` for `barbecue`.

Unicode-letter, lowercase-ASCII and uppercase-ASCII delimiters now all work, and
so do nesting (`α α 5 ω ω`) and full expressions inside the brackets
(`α 5 + 3 ω`). Pinned by `t/custom-operator-and-term-parsing.t` section 4.
