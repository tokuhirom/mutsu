# Attribute defaults bind the lexicals of their declaring scope

An attribute default such as `has $.n = ++$count` used to read and write the
outer lexical by name in whatever env the object was constructed in. A write
landed in a copy, so `$count` itself never changed. A class declared in a
block and constructed after the block saw `Any`. A constructing routine's
own `$count` was mistaken for the declaring scope's (#13340).

Each plain lexical that a class or role attribute expression names is now
boxed into a shared cell at its declaration, the way a method body's writes
already were. Registration captures that cell, and the default resolves the
name through it ahead of the constructing scope. So writes reach the
lexical, reads see its later writes, and the block lexical outlives the
block. A role declared in a block whose methods read the block's lexicals is
still wrong; that is #13358.
