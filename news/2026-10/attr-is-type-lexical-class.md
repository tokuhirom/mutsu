# `has @.a is Type` with a lexical class builds the type

A container-type trait naming a `my class` (`has @!idents is Buffer` where `Buffer` is lexical)
fell back to a plain Array, because construction looked the bare name up in the global registry.
The trait is now resolved to the class's storage name while the lexical is in scope.
Found on the `annotations` distribution (`Keeper` in `t/01-containers`).
