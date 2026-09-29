# Numeric associative keys on objects

Brace subscripts on objects with a user-defined `AT-KEY` now pass numeric keys to that method. Integer keys no longer fall through to the inherited positional accessor, and fractional keys retain their original value.
