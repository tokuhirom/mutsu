# `Parameter.default` and `.constraints` closures run a shared compiled body

The code objects `Parameter.default` and `.constraints` hand back (a default
thunk, a `where { }` block, a `where * > 10` WhateverCode) carried an AST body
the tree-walk carrier of `call_sub_value` recompiled on every call. They now
share one compiled chunk per body text. The value layer builds them, so the
runtime installs the compiler on it once (`install_synth_compile`) instead of
the value layer naming the compiler. Roast's `S06-signature/introspection.t`
no longer reaches the carrier, which leaves no whitelisted roast file on it. A
slice of #14572.
