# An exported multi can take a module's `my role` as a parameter type

A `my role` / `my class` is stored under a declaration-site key (`R\0<id>`), and only
its declaring scope's env maps the source spelling to it. A `multi sub ... is export`
whose signature named such a type was matched later in the caller's env, where the bare
name resolved to nothing, so no candidate matched (`Cannot resolve caller ...`). When a
bare constraint fails to match and exactly one lexical type carries that name,
`type_matches_value` now retries against that storage key.

Found through the Zef distribution P5print (`multi sub print(P5Handle $handle, *@_)`
called with `$*OUT but P5Handle`). The rest of its `t/01-basic.rakutest` needs #11054
(a class method assigning an outer lexical inherits the caller's readonly parameter mark).
