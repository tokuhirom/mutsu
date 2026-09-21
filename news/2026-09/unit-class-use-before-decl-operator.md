# Operators imported before a unit declaration are visible in the unit class

A file-level `use` that precedes `unit class` now installs imported operators
in the unit class's package scope. This matches Raku's compilation-unit
semantics and lets `Arithmetic::PaperAndPencil` call the operators exported by
its `Number` module from its own methods.

Exported operator subs declared in a unit class also retain that class as
their lexical owner for private-method calls on typed operands.
