# A class declared in a block that imports something outlives the block

A class is `our`-scoped: declared in a block, it belongs to the package and
is still there after the block exits. But in a block that also `use`d or
`import`ed a module, mutsu removed every class the block registered when the
block exited, because the import scope rolls the class table back (ADR-8751).
`{ import Foo; class C does Bar { } }; C.new` then died with "No such method
'new'". Roast `S11-modules/export.t` declares its classes this way, inside
such a block, and only tests them within it.

`register_class_decl` now records an `our` class that the running code
declares inside an open import scope in `persistent_classes`, which the
rollback keeps. Roles and grammars follow the same rule. A `my class` stays
lexical to its block, and a module that loads while the block runs still
declares its own types through `module_declared_types`.
