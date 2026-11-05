# RakuAST Doc::Block builders and config queries

`RakuAST::Doc::Block` gains the in-place builders `set-type`, `set-level` (an
`Int`; `0` is no level), `set-margin`, `set-paragraphs` (a list, or a lone
value as a list of one), `add-paragraph`, `set-config` (a map or a list of
pairs; `Nil` is accepted) and `add-config` (the last value for a key wins),
and the queries `config-keys` and `config-pairs` (`List`s, in key order) and
`leading-whitespace` (the start of the first paragraph; `Any` when there is
none). A node edited this way prints its fields in the order the constructor
builds them. All measured against rakudo 2026.09. A repeated named argument to
a constructor, which keeps the first value here and the last in rakudo, is
filed as #16762.
