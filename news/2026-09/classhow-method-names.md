# `.^method_names` on classes

`Metamodel::ClassHOW.method_names` (`A.^method_names`) now returns the names
of the methods, accessors and submethods a class declares itself, as Rakudo
does. Found while working the `Statistics::Distributions` ecosystem
distribution, whose `mixture-dist` checks `'generate' ∈ $_.^method_names`.
