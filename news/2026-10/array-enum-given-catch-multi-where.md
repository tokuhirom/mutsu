# `my Array enum` members act as arrays; `with`/`given` + CATCH keeps its value; multi-method `where m//` captures

Found running PDF::Content's own suite:

- A member of `my Array enum` matches `List`/`Array`, answers the base's collection methods and
  subscripts, and spreads under `|`.
- A `with`/`given` body that contains a `CATCH` no longer drops its tail value (the implicit try
  netted one value, which the body then popped).
- The `$/` set by a `where m/.../` on the chosen multi method candidate is visible in its body.
