# A type name before a user circumfix's closer parses

`[@ Int @]` and `<@ Int:D @>:name<X>` (Data::Record's literals) died with
"couldn't find final '@]'": the listop-call heuristic read `Int @]` as a call
of the type with the closer's `@` as an anonymous-array argument. A type name
followed by a bare sigil that ends at a closing delimiter (or whitespace) is no
call any more, so the circumfix parser sees its closer. `(Int @)` is "two
terms in a row" as in raku. Refs #15587.
