# A Map-defaulted hash attribute accepts element assignments — DBIish 36-pg-enum 13 → 25/26

2026-07-29. `DBDish::Pg::Connection` declares
`has %.dynamic-types = %oid-to-type;` where `%oid-to-type` is a `Map`
constant, and `t/36-pg-enum.rakutest` later does
`$dbh.dynamic-types{$oid} = YesNo` — which died with the telltale
"Type check failed for an element of %; expected  but got Package"
(note the EMPTY expected type), stopping the file at 13 of 26.

Root cause: a `Map` carries embedded container metadata (`declared_type:
"Map"`, no value type). When the attribute-subscript assignment path
(`builtin_index_assign_method_lvalue`) consulted that metadata,
`hashdata_type_info` rendered the absent value type as an empty string, and
the element type check ran against `""` — rejecting every value. The check
now treats an empty `value_type` as "no element constraint", alongside the
existing `Mu`/`Any` exemptions.

With the fix the file runs 25/26. The remaining fail ("Value OK (No eq
Yes)") is an unrelated closure-capture staleness: a converter sub stored via
`$dbh.Converter{YesNo} = $yesno` reads `$expected` as captured at store
time, missing the mainline's later `$expected = 'No'` write (the minimal
`$k.c{Str} = sub ...` shape also trips an "Impossible coercion from 'Str'
into 'Any'" on the type-object hash key — both remain in the parity ledger).

Pin: `t/hash-attr-map-default-element-assign.t` (passes under raku too).
