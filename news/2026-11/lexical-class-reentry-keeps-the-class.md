# A `my class` in a routine is registered once, not on every call

A lexical class declared inside a routine body ran its full registration on
every entry of that routine: compose roles, rebuild every method row,
recompile the method bodies, and bump `Registry::method_generation`, which
discards every method-resolution, multi-dispatch and regex call cache in the
program. In rakudo the declaration is compile-time: `sub f { my class C {}; C }`
returns the same type object on every call, and only the class body's
statements and the methods' closure over the current frame are per entry.

CSS::Module's grammar actions do exactly this (`method build { use
CSS::Grammar::AST; my class builder is CSS::Grammar::AST { ... } }`), so each
action call re-warmed those caches from scratch. A declaration whose body is
purely declarative (`has`/`method`, no custom traits, metaclass or per-entry
parent arguments) is now stamped with its site and resolved parents when it
registers, and a later entry with the same stamp keeps the class and only
re-captures its methods' outer lexicals (`runtime::registration_class_site`).
As a side effect a method added through the MOP now stays on the class across
calls, as on rakudo.

`Enumeration.enums` also builds its `Map` natively instead of dispatching
`Map.new` (an MRO walk) per call.

`CSS::Stylesheet.parse` of CSS::TagSet's 39-line `tagged-pdf.css` went from
~6x rakudo to under 3x (#16978).
