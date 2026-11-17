# `is raw` and sigilless method parameters take the fast binder

A method whose parameter binds the caller's container (`Mu $x is raw`, a
sigilless `\x`) used to send every call with a variable argument to the general
binder, which realizes the alias by name and writes it back on exit. The method
fast binder now binds such a parameter through the container transport ADR-0109
gave `is rw` on the sub path: the caller's variable is promoted to its shared
cell before the method frame is pushed, a `Proxy` argument is relayed as
itself, and a value binds as a read-only term. Argument shapes the
classification cannot settle (attribute or dynamic sources, by-name aliases,
captured outer variables, a body that may `callsame`) keep the general binder.

Callgrind, per call, profiling build:

| call | before | after |
| --- | ---: | ---: |
| `$o.raw($v)` (`Mu $a is raw`) | 42.7k Ir | 21.1k Ir |
| `$o.is-set($v)` (`Mu \obj`) | 49.4k Ir | 21.5k Ir |
| `$o.build-attr($v, $v)` (two `is raw`) | 63.4k Ir | 30.8k Ir |

AttrX::Mooish's own loop barely moves (7.79M to 7.73M Ir per iteration): its
`is raw` arguments are mostly the FETCH closure's captured variables, which
have no caller slot to promote. ADR-15737 D1, refs #15737.
