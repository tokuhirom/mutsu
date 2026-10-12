# A multi sub's `where m/.../` sets `$/` for its body

`multi sub a(Str $s where m/^(\d)/) { ~$0 }` printed an empty string with a "Use of Nil"
warning, while a plain sub and a `multi method` saw the capture. Multi resolution runs the
`where` in a scope that discards `$/`, and the winner of a value-dependent multi then took the
positional light-call bind, which trusts that verdict and never runs a `where` again. A regex
`where` is now never treated as verified for the light bind (`ParamDef::where_is_regex_match`,
shared with the general binder's re-check), so the winner binds through the general binder,
which re-runs the side-effect-free match and leaves `$/` for the body (#12915).
