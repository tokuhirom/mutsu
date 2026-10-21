# A regex literal closes over a top-level kebab-case lexical

`/$cont-name \s container/` at the top level of a script captured nothing for
`$cont-name` (a short name such as `$contname` was captured), so a routine that
isolates itself from its caller's names, like `Test::cmp-ok` with its
`&CALLER::LEXICAL::(...)` lookup, matched the regex against an empty
interpolation. A kebab-case lexical is now captured from its slot at the top
level too. This was the "Scalar container" failure in AttrX::Mooish
`t/010-base`.
