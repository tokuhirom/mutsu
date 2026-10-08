# RakuAST: `given EXPR -> PARAM { }` as a PointyBlock

`given X -> $v { }` (and `-> \v`, `is rw`/`is copy`, typed and destructuring
parameters) now crosses the RakuAST boundary as `Statement::Given` with a
`PointyBlock` body, as rakudo renders it. The parser's parameter bind stays the
execution form; the written parameter and body ride along as a
`SourceForm::GivenPointy` record, which `lower` hands back to the same
`given_pointy_body` expansion. The `MUTSU_RAKUAST=1` ratchet gains 25 files.
