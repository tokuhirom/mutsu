# Reject invalid List subscript operations

Deleting an element of an immutable List now throws instead of silently continuing. A Pair used as an index in a positional List or Array slice now reports the missing Int coercion rather than producing a missing element.
