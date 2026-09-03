# RakuAST nodes have stable identity

RakuAST nodes now retain object identity across aliases. Their `===` and
`.WHICH` comparisons use the shared model-node allocation, so cloning the
containing `Value` does not make the node appear to be a new object.

`eqv` compares RakuAST nodes structurally, including their class, field names,
and recursively equivalent child values. Separately constructed copies of the
same tree are therefore `eqv` but are not `===`, matching Rakudo.

The dual-oracle regression coverage is in
`t/rakuast-identity.t`.
