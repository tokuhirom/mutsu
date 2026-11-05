# An imported role named like a core type satisfies that type constraint

A module that exports `role Variable` (or any user type spelled like a core type) now has its instances accepted by `my Variable $x`, parameters and attributes constrained with that name, also when the importing module uses the bare word in a method body. The import alias is consulted only after the core meaning rejected the value. Java::Generate `t/03-operators.t` no longer dies on `has Variable $.left`.
