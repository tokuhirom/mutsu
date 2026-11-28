# A topic handed to a sigilless parameter takes the light call path

`f($_)` into a sigilless (`\v`) parameter always took the general binder: the positional-light
path's sigilless bind refused a `$_` source outright, leaving the topic's rules to the general
binder. JSON::Fast's encoder makes this call for most values it writes: `jsonify($_)` in its
`for` loops, and `str-escape($_)` and `pretty-associative($_)` inside `with obj { ... }`.

The light bind now serves the topic itself:

- a topic that is a cell (an element of a `for` over an Array, a hash's values, a `with`
  block's container) is bound as that cell, so a write reaches the element, as the general
  binder's shared-cell rule does;
- a topic that is a plain value (a `for` over a List's values, a `given` over a computed value)
  is bound as that value. It is read-only, as in rakudo.

That last point is also a fix. The general binder bound such a value topic writable, so
`sub write(\v) { v = 'w' }; write($_) for 1, 2` succeeded where rakudo says "Cannot modify an
immutable Int".

`bench-json-fast-encode` (`scripts/bench-det.sh`, `+jit`, local A/B on b882b8da8):
846.3M -> 771.3M instructions (-8.9%), 349,821 -> 260,550 allocations (-25.5%), and the warm
section 0.093 s -> 0.083 s (median of 5).
