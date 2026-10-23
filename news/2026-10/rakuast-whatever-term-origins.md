# RakuAST terms holding a curried `*` keep their origin

A term that holds a `*` the whole-unit currying pass turns into a
`WhateverCode` (`.map(* + 1)`, `@a.map(*.abs).sum`) lost its `.AST` origin,
and so did every term around it: the parser notes a term's range with a
fingerprint of what it read, and the currying pass, which runs before the
ranges are attached, wraps the curried expression in a `WhateverCurry` marker
and turns its `*` into the code's argument. The fingerprint no longer matched,
so a multi-line method chain with a `*` in it reported none of its lines to a
`visit-children` walker such as `Code::Coverable`.

Term fingerprints now read through the marker and treat the curried argument
as the `*` the parser read, so those terms find their ranges again. The
curried `*` itself still takes its note but attaches nothing, since Rakudo
gives `WhateverCode::Argument` no origin.
