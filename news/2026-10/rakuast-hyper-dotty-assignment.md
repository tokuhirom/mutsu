RakuAST now round-trips hyper call assignments to nested subscripts such as
`@b[0]>>.=uc`. Conversion restores Rakudo's `ApplyDottyInfix` shape from the
parser's indexed writeback expansion, and lowering reuses the existing `.=` path.
