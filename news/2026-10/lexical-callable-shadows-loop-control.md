# A lexical `&next` / `&last` / `&redo` is called, not read as loop control

`-> &next { next() }` (a pointy block, like `sub f(&next)`) and `my &next = ...; next()` now call the
lexical routine instead of parsing `next()` as the loop-control statement, which died with "next
without loop construct". BDD::Behave's config `around-each` chain hung forever on it
(`t/configuration/runner-integration.rakutest` now passes in about 10 s on a debug build).
