# Slang tokens can read the core `sub` / `method` definition with `key-origin`

A slang token that writes `<routine-def=.key-origin('routine-def', 'sub')>` or
`<method-def=.key-origin('method-def', 'method')>` (Slang::Mosdef, EEL, Qwiratry) now gets the
`RakuAST::Sub` / `RakuAST::Method` the core rule builds for the rest of the declaration after the
slang's own keyword (#13370, #16888). A production whose slang attaches no node (Mosdef has no actions
role) stands for the core definition it captured, as rakudo's own `routine-declarator` action makes it.
`class Bar { def bub { return 12 } }` and `fn twice($n) { $n * 2 }` with a slang keyword now work.
Slang::Mosdef itself still stops at the anonymous `λ ($n) { ... }` form.
