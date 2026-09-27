# A `die` escaping `quietly` no longer silences every later warning

`try { quietly { die 1 } }; warn "after"` printed nothing: the warning was
swallowed, and so was every warning for the rest of the process. `quietly { … }`
compiles its body inline between a `WarnSuppressPush` and a `WarnSuppressPop`.
An error unwinding out of the body skips the pop, so the suppression depth
stayed raised.

Every such unwind ends at a try/CATCH region, and `quietly` always nests
lexically inside that region. So the try/CATCH opcode now records the
warning-suppression state on entry and restores it on every exit. An enclosing
`quietly` stays in force, and a `die` inside `quietly` still dies.
