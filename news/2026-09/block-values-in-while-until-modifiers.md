# Keep bare blocks as values in while and until modifiers

A bare `{ ... }` before a `while` or `until` statement modifier is now compiled
as a Block value. The loop still evaluates its condition, but it does not
invoke the block body. Prefix loops and other statement modifiers keep their
existing block behavior.
