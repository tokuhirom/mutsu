# `^add_multi_method` closures keep the declaring module's imports

A method closure a module handed to `type.^add_multi_method` was registered without its
compiled code, so the first call recompiled the body from its AST with no memory of the
module's `use` lines. A routine the module imported (`ast-value` in Red's `Comparate`, whose
`STORE` Proxy builds the value of a column assignment) then died with `Unknown function`
once the method ran from another file's frame. `^add_method` already carried the closure's
own compiled code; `^add_multi_method` now does too.

In Red 0.2.7 this was `JsonDocument.^all.map({ .json-column = {...} }).save`
(`t/87-json-operations`, `t/13-result-seq-update`).

Refs #14094.
