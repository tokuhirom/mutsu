# `%!x = %y` meets its instruction budget

#15444 asked that `method set(%ch) { %!children = %ch; self }` add at most 10k
instructions over `method empty(%ch) { self }`, from 31.8k/+30k at the time it
was filed. The scalar half landed earlier (`$!key = $k`, 26.3k -> 18.8k); this
closes the hash half. Measured with callgrind on the `profiling` build, second
run, 2000 calls, A/B from the same checkout:

| | Ir per call over the empty body |
| --- | ---: |
| `main` at the start of the 8th perf session | 18.2k |
| attribute resolved once, cell store through the inline cache (#15529) | 11.75k |
| plain Hash into an untyped `%!attr` skips the coercion probes | 10.4k |
| the prefetch takes the invocant once | **9.85k** |

- `coerce_hash_var_value` answers a plain Hash assigned to an untyped, unconstrained
  `%!attr` whose current container is a plain Hash (no value or key type, no
  `is default`, no typed keys, no container-valued entries) with the Hash itself:
  every probe it would run answers no for that shape, and its final coercion
  returns a Hash unchanged. Any other shape (a typed attribute, an object hash, a
  Map or Bag, a list, a bound cell) takes the general path as before.
- The prefetch of the attribute cell and its declared type resolves `self` once
  and shares it with the no-`self` check, instead of three `get_env_self` calls.

A hash attribute's `is default(...)` is lost by `%!h = %x` before and after this
change; that is a separate bug, filed as #15547.
