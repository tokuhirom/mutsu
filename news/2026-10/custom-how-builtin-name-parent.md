# Custom HOW parents work when a class shadows a builtin name

A custom `ClassHOW` can now add a parent while composing an `EXPORTHOW::DECLARE` class whose name matches a builtin type. The new parent stays ahead of the synthetic `Any` root, preserving a valid MRO and the declared class's instance behavior.
