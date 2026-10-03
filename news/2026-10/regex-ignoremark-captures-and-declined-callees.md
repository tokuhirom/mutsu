# `(:m …)` capture groups compile, and calls of declined rules stop bridging to the walk

Two more pieces of ADR-0135's walk residue are gone (Slice E, fourteenth part).

A capture group whose body is scoped `:ignoremark`, such as JSON::Tiny's `(:ignoremark '"')`, made
its whole pattern fall back to the regex tree walk. It now compiles: the group's ends come from the
mark-stripped subject, as `[:m …]` already did. A call of a rule that has no compiled program, most
often a `:m` rule, is now evaluated by the compiled engine's eager call path instead of the walk's
producer.

Across `t/grammar`, `t/regex` and `t/modules`, the walk is used 626 times instead of 6,347: whole
matches it answered fell from 3,164 to 488, and calls bridged to it from 3,183 to 138. JSON::Tiny's
parse now uses no walk at all.
