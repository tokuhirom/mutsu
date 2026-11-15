# ObjAt subclasses inherit the positional constructor

User subclasses of `ObjAt` and `ValueObjAt` now accept the identity text
through `.new(text)`. The VM and ordinary constructor dispatch share the
native builder. The resulting instance keeps its subclass type through
`WHICH`, and inherited string and Raku rendering use its identity text.
