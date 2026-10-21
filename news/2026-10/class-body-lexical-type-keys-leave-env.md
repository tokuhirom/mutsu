# A class body's `my role` / `my class` leaves no type key in the loader's env

A `my role` or `my class` declared in the body of a module's class bound a
type-only key (`lexical_type_key`) in the loading program's env, and the class
body exit never took it out again. After `use NativeCall` that left
`IntTypedCArray` and its siblings in every copy of the program's frame (#7817).
The exit now restores that key together with the short name it already
restored; the class's methods still resolve the role through the owner package
chain (#14331).
