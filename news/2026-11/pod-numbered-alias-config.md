# The `#` pod alias is `:numbered(1)`

`=head1 # Title`, `=item # Text`, `=para #` and the other abbreviated blocks
carry `numbered => 1` in their config, as rakudo does, and the `#` is no longer
part of the text; mutsu stored `True` (and nothing at all for an `=item`).
An explicit `:numbered` config option still reads `True`. Found by running
`raku-doc` code blocks against mutsu and `raku`.
