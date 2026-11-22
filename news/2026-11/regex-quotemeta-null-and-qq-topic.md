# Regex: `\Q` is obsolete, `//` is a null regex, `"{ $_ }"` reads the regex's topic

Three of roast `S05-mass/rx.t`'s remaining gaps (#18738):

- `\Q…\E` (Perl 5 quotemeta) throws `X::Obsolete` with rakudo's wording,
  instead of `X::Backslash::UnrecognizedSequence`.
- `//` in term position (`"a" ~~ //`) is an empty regex, which rakudo rejects
  as `X::Syntax::Regex::NullRegex`. mutsu reported a confused parse.
- A `"..."` atom whose code block reads `$_` (`/ "{ $_ }" /`) never ran the
  block. The atom's compiled thunk was refused for any body naming `$_`,
  because the thunk could not see the topic the regex runs under. The thunk
  is a block, so it is now called with that topic as its `$_`: the subject of
  `~~`, or the caller's `$_` for `.match`/`.subst`, as in rakudo.
