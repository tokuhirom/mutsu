# A call to a routine with a sigilless parameter is no longer statically linked to TRIR

`sub f(\x)` called with a plain lexical (`f($n)`) compiled to a `CallTrir` site whose direct
bind always declined (a sigilless parameter binds the caller's container), so every call took
the cold by-name fallback. The compiler now leaves such sites to `CallFunc`. A
`sub g(\x) { 1 }; g($v)` call went from ~10 us to ~7 us; the remaining cost is the container
binding itself (#17376).
