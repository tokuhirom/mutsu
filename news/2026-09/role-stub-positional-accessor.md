# A non-multi role stub with positional parameters is satisfied by an attribute accessor

`role K { method r($x) { ... } }; class E does K { has $.r = 1 }` used to die
at composition with "Method 'r' must be implemented by E because it is
required by roles: K." even though `E` provides a concrete `r` via its `$.r`
accessor. `resolve_class_stub_requirements` already satisfies a non-multi
stub by name for a concrete method, an inherited method, or a
token/proto — a non-multi stub's signature is advisory in rakudo — but the
attribute-accessor branch still required the stub to be nullary. It now
follows the same by-name rule, so any public attribute of the stub's name
satisfies it regardless of the stub's positional signature. A stubbed
*multi* is unchanged: rakudo keeps per-candidate signature enforcement
there, so an accessor only satisfies a multi stub when it is nullary
(#9758).
