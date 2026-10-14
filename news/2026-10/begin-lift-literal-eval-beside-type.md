# A nested BEGIN with a literal EVAL beside a type is lifted

A `BEGIN` inside a routine that used `EVAL` in a scope declaring a type was
never run at compile time: the EVAL text might name the type and change it
through its metaobject, which a lifted copy of the type would lose. Because a
non-liftable BEGIN halts every later nested BEGIN, `sub f { class K { }; BEGIN
say EVAL q[K.^name] }; sub g { BEGIN say "b" }; say "m"` printed only `m`
instead of `K b m`.

When the EVAL text is a string literal, the lift now parses it and reads what
it names and does from the tree: read-only metamethods lift, a changing one
(`.^add_method`) still keeps the BEGIN in place (ADR-0134, #13096).
