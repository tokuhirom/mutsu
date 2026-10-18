# `STORE` on arrays, lists, hashes and maps follows Rakudo

`.STORE` on the built-in containers is now one set of rows (ADR-11276 §9.174), and several answers
changed to match Rakudo:

- `%h.STORE(keys, values)` pairs each key with its value.
- A typed array checks every element it stores.
- `Map.STORE` refuses a `Map` that is already built, unless called with `:INITIALIZE`.
- `(1, 2).STORE(...)` raises `X::Assignment::RO` for its first element.
- Through a `$` variable, the answer is the container itself.
