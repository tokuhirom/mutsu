Parsed RakuAST trees now record sink context for non-final statements and
propagate it to their expressions and block bodies. `sunk`,
`is-block-statement`, and `dump` report the resulting markers, while
`SemiList` statements remain outside block-statement context (#19106).
