# `&?ROUTINE` inside a grammar rule is the rule itself

`&?ROUTINE` in a rule's code block or call argument used to be the routine the
parse was entered through (`outer` while matching `inner`, the proto `b` for a
`b:sym<t>` candidate), so `&?ROUTINE.WHY` was empty and CSS::Module's usage
text came out blank. A rule's `&?ROUTINE` is now resolved at declaration time,
as Rakudo does, to the grammar's own lookup of the rule (the candidate for a
proto `:sym<>`). No routine frame is pushed per rule call, so the grammar
parse benchmarks are unaffected (#13246).
