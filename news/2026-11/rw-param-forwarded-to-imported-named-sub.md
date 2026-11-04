# An `is rw` parameter forwarded to an imported sub keeps the callee's write

A routine called through its lexical import (`use Module; sub w($p is rw) { imported($p) }`)
bound the caller's `is rw` parameter to a freshly boxed cell, because the call site
never published the caller's shared cell for the binder the way the by-name call sites
do (`publish_rw_arg_cells`). A callee that also takes a named parameter reaches that
route, so TOML's `parse-integer($t, $pos)` left `$pos` at 0. The imported-routine call
now publishes the cells first. Fixes #16377.
