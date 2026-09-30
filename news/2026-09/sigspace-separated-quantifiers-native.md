# Sigspace separated quantifiers are parsed natively

Under `:sigspace`, a separated quantifier such as `a*? % ","` used to be
rewritten to text by the LTM expansion before parsing. That rewrite lost a
frugal quantifier's priority (`"a, a, a" ~~ / :s a*? % "," /` matched `a`
instead of the empty string, and `a**?2..3 % ","` took three items instead of
two) and leaked `ws` captures into the Match (#10339).

The separated quantifier now goes through the per-token parser, which places
the significant whitespace the way Rakudo does: whitespace after the separator
atom becomes a `<.ws>` inside the separator, whitespace between the quantifier
and `%` becomes a `<.ws>` after the whole quantifier, and whitespace between an
atom and its quantifier (`<alpha> +% \,`) becomes a `<.ws>` after the atom in
every iteration. The last rule also applies to unseparated quantifiers.

Along the way, a capture name left on a separated token now names each item,
as it does without a separator: `"1,2" ~~ / <digit>+ % "," /` gives
`$<digit>` two Matches instead of one spanning `1,2`. A sigil alias of the
whole span (`$<x>=\d+ % ","`) is wrapped in a group by the parser and still
captures the span.
