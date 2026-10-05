# Native-declared integer parameters unbox and wrap

A parameter typed by a user-declared `native` integer now binds through the core integer type
matching its recorded REPR, width and signedness. `Bool` is unboxed to its integer value, and a
value such as `300` wraps to `44` for an 8-bit signed declaration. Routine and method parameters
share the same binding behavior.
