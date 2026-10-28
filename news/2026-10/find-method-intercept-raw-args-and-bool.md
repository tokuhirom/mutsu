# `^find_method` types: raw arguments alias the caller, `?$obj` asks `.Bool`

A call answered by a user `^find_method` lost the caller's argument sources
(the metamethod bound its own arguments first and consumed them), so a found
method's raw `\o` parameter received a copy instead of the caller's variable.
The sources and containers are now kept for the found method. Prefix `?` on an
instance of such a type now asks the user's `.Bool`, as rakudo does. Found with
Object::Trampoline, whose `t/01-basic.rakutest` now matches raku. Known
difference: `so($obj)` is rakudo's `.so` (the real object), and still asks
`.Bool` here.
