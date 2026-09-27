# A role stub with an explicit invocant is satisfied by an attribute accessor

`role I { method r(::?ROLE:D:) { ... } }; class C does I { has $.r }` used to
die at composition with "Method 'r' must be implemented by C because it is
required by roles: I." The check that lets a public attribute's accessor
satisfy a nullary stub counted the explicit invocant as a positional
parameter, so the stub looked like it took an argument. The invocant is now
ignored there, matching rakudo. This unblocks `Data::Record::Map`, whose
`has %.record` implements `Data::Record::Instance`'s
`method record(::?ROLE:D: --> T) { ... }` (#9727).
