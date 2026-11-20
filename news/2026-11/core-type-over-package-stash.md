# A CORE type name inside a package no longer picks up a same-named nested package

Inside package `P`, a bare type name such as `Supply` in a parameter, a
return type or a typed variable means the CORE type whenever CORE has one,
even when a `P::Supply` package exists (rakudo finds CORE lexically before it
consults the package's stash). mutsu's parse-time type qualification decided
"is this a CORE type?" from the builtin type catalog alone, which lacks
`Supply`, `Thread`, `Signature`, the core roles and others, so it rewrote
`--> Supply` to `--> P::Supply`.

Red 0.2.14 hit this: `Red::ResultSeq` `use`s `Red::ResultSeq::Supply` and
declares `method Supply(--> Supply)`, so every `.Supply` call died with
"Type check failed for return value; expected Red::ResultSeq::Supply but got
Supply" (#18386). The check now also consults the parser-level known-type
table.
