# Callable constraints recognize pointy block signatures

A Callable parameter with a signature constraint now reads the effective
signature of a pointy block. A simple `-> $n { ... }` block keeps its parameter
in a compact representation, which previously made the constraint checker see
an empty signature even though `.signature` displayed the parameter correctly.
Bare blocks retain their separate implicit-topic signature behavior.
