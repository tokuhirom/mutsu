# `UNIT::name` names a lexical class of the compunit

`UNIT::errno` inside a module file whose `my class errno` is lexical died with
`Could not find symbol '&errno' in 'UNIT'` (the lowercase-last-segment branch
took it for a sub), and read from a closure that outlives the load it found no
binding. A `UNIT::`/`MY::` name that denotes a lexical class declared in the
compunit now compiles to the by-name class read, which closures capture.
Found with Unix::errno, whose `t/errno.rakutest` now matches raku (its
`t/01-basic.rakutest` still differs: it depends on `MY::«&item:<errno>»:exists`
returning a two-element, truthy list in rakudo).
