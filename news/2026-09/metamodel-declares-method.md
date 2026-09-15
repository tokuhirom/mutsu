# Class and grammar metamodels implement `declares_method`

`Metamodel::MethodContainer.declares_method` now reports whether a class or
grammar directly declares a public method, submethod, accessor, composed role
method, or grammar token. It deliberately does not walk inherited methods and
does not expose private methods.

This unblocks Red's model composition from using `.^declares_method` to decide
whether it needs to wrap a model's `BUILD` or `TWEAK` method.
