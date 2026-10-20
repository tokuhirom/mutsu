# Dead fix-ups after the native method entry are gone

A coverage probe over `t/` and the whitelisted roast files showed four blocks of
`vm_native_dispatch::try_native_method_raw` that nothing reaches any more: the
second table lookup after stripping an undeclared named argument, and the
post-fixes for `.List`/`.values`/`.Slip`/scan coercions of a `gather` or scan
`LazyList`. The rows and the lazy step answer those calls; the blocks are
deleted and `t/oo/method/lazy-coercion-method-rows.t` pins the answers.
