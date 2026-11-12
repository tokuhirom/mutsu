# Built-in payload keys are read by symbol

A user class deriving from a built-in value type (`class MyInt is Int`,
`is Pair`, `is Range`, `is Capture`, ...) keeps its payload in a reserved
attribute, and a native-method probe asks every instance it meets whether it
carries one. Those probes looked the reserved names up as strings: up to a
dozen `Symbol::lookup` string hashes per probe, and a single method call on an
ordinary object runs the probe several times.

The readers now use well-known symbols (`wk::int_payload()`,
`wk::seq_payload()`, ...), interned once per process. A unit test pins each
symbol to the string constant the payload is stored under.

Instructions per call on a plain two-attribute object (release build,
callgrind): `.clone` 111k to 95k, `.defined` 32k to 26k (#17530, EEL's
per-fork state copier).
