# `MY::` lists the variable the reading statement declares

A `my` declaration is in the block's pad from the start of its own statement in raku, so
`my @n = MY::.keys.grep(...)` lists `@n`. The compiler now notes the variable a `my` declares
after its initializer, and the baked `MY::` frame spec includes it (#15607).
