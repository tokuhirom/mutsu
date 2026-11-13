# A sigilless term bound to a subscript or call result declares through the env-free lane

`my \h = $cache{k}` and `my \m = f(...)` bind the term to whatever the subscript or
call hands back. Their declaration ran the general bind store (env mirrors, package
probes, alias resolution) even when nothing outside the routine could name the
slot. Such a slot now takes the env-free declaration lane when the bound value is
plain (ADR-17376 amendment): `sub f(Mu \m) { my \k = m{'a'}; 1 }` went from 33.5k to
18.5k instructions per call, a Monad-shaped module helper from 63.2k to 60.2k
(#17376, #17331).

A slot-only term's "Cannot modify an immutable" error now names the value
(`Int (5)`) rather than the term (`\x`). Filed on the way: #17677 (a readonly bound
term accepts `is rw` and `+=`), #17678 (`my \r = f($x)` with an `is raw` routine
does not alias).
