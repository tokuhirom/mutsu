# Closures read their free `@`/`%` lexicals by upvalue

A closure that only reads an `@array` or `%hash` of an enclosing scope now
captures it by upvalue index, as it already did for a `$scalar`
(ADR-0039 §4.2, #9895). Its reads no longer resolve the name through the
running frame's env chain on every access, so they cannot pick up a caller's
same-named container either. Role type parameters (`role R[@l]`, bound when
the role method runs) and `%$h`-style contextualizer reads keep the by-name
path. The env mirror on container stores, the issue's other half, is still
to come.
