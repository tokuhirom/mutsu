# Topicalizers ask about the topic by its well-known symbol

`given`, `with`, `for`, smartmatch and `s///` save the topic `$_`, change
whether it is readonly, and restore both afterwards. Each step probed the
env and the readonly set under the text `"_"`, so a single `given` interned
the name about nine times. The `given`/`with` op now uses the topic's
well-known symbol throughout. The `&str` readonly accessors answer `_` from
that symbol too, which covers every other topicalizer. A sigil-less
`declared_scalar_type` lookup also no longer probes the same key twice
(#18274).
