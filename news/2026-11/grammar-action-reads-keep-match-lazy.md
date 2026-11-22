# A grammar action's reads of `$/` no longer materialize the Match

A lazy `Match` answers `.Str`, `.from`, `.to` and its captures from the
capture tree; building its attribute map is meant to happen only when
something really needs the map. In grammar-action code it happened once per
action anyway: `~$/` probed its operand with `view()` for the Buf, code and
nested-Seq cases, `$<name>` read the named-capture hash out of the map, and a
method call on a capture passed a dozen receiver probes (buffer class, payload
subclass, user-override lever, proto body, junction, Proxy, Failure, ...) that
each decoded the full view. Each probe now answers a lazy Match from its tag
or its cursor class, and `$<name>` reads the capture node directly.

A `make ~$/` action dropped from ~33.6k to ~27.9k instructions, and a warm
YAMLish `load-yaml` of 60 rows from ~159.5M to ~142.4M (−10.7%; Match
materializations 4,232 → 250 per two parses). On the local box the parse with
actions now runs at ~19 ms against rakudo's ~20 ms (#12917).
