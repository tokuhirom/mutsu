# `"@*ARGS[0]"` and `"%*ENV<HOME>"` interpolate in double-quoted strings

A dynamic (`*`-twigilled) array or hash variable followed by a subscript was
left literal inside a `qq` string: `"v=@*ARGS[0]"` printed `v=@*ARGS[0]`
instead of `v=x`, and `"%*ENV{'HOME'}"` was worse — the `%*ENV` part stayed
literal while the `{'HOME'}` was interpolated as a block, giving `%*ENVHOME`.

The quote-slang scanner's `split_interp_var_name` accepted only the attribute
twigils `!` and `.` after an `@`/`%` sigil. It now accepts `*` as well, so the
name becomes `ArrayVar("*ARGS")` / `HashVar("*ENV")` — the same node the
non-interpolated parser builds — and the subscript, zen-slice and
`.method()` forms all follow. A bare `@*foo` / `%*foo` with no postcircumfix
stays literal, as in Rakudo.

Pinned by `t/lang/interp-dynamic-twigil-subscript.t` (#9166).
