# `*<a>:exists` and `*.<a><b>:exists` are WhateverCode

A WhateverCode whose postfix chain ends in a subscript with an `:exists` / `:!exists` adverb used
to evaluate the adverb on the priming closure (`(%h,).map(*.<a><b>:exists)` died with "Cannot map
a List using 'Bool::True'"). The compiler now moves the priming scope around the adverb, and the
WhateverCode arity / placeholder walks look through `Expr::Exists`. The value adverbs
(`*<a>:k`, `:v`, `:p`) still evaluate eagerly; that is a separate gap.
