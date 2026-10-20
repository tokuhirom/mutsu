# `Any.push(1)` on the bare `Any` word is refused

The parser folds the `Any` keyword to a type-object literal, which the compiler sent down the generic method path with no target name, so the growing rows answered a fresh temporary array and `Any.push(1)` succeeded silently. The literal now takes the mutating entry under a term key, as `my \s = Any; s.push(1)` always did, and Rakudo's `X::Assignment::RO` ("Cannot modify an immutable 'Any' type object") is raised. Scalars named `$Any` or typed `Any` still autovivify.
