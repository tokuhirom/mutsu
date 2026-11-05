# A module's constant from its own class resolves after another unit module shares the namespace

A type nested under the loading module's own name now claims its package for that module when it is declared, not only after the load. A top-level `constant` initialised from such a class (`my constant MS = Zz::Min::Sym;`) no longer dies "Could not find symbol" when a `unit module` with the same first `::`-segment was `use`d before. Mathematica::Serializer `t/03-wxf-deserializer.rakutest` passes.
