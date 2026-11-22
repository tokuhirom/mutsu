# `*.abs.WHAT` is `(WhateverCode)`

The pseudo-methods that never prime (`.WHAT`, `.WHO`, `.HOW`, `.VAR`,
`.REPR`, `.DEFINITE`, `.WHERE`) were exempt only when called directly on `*`
or on an already-parenthesized WhateverCode. After a priming chain they joined
the curried body, so `say *.abs.WHAT` printed `*.abs.WHAT` (the gist of a
WhateverCode) instead of `(WhateverCode)`. The chain before such a call is now
finished into its WhateverCode first, as in rakudo. `.REPR` joins the list:
`*.REPR` is `P6opaque` (#18514).
