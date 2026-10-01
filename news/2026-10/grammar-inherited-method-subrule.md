# Inherited grammar methods resolve as `<.name>` subrules

A method declared on a parent grammar (or composed from a role) was invisible when
a rule was evaluated in a derived grammar, so `B2.parse("a")` with `grammar B2 is B1`
failed whenever an inherited token called `<.acc>`. The subrule-as-method predicate,
the compiled call-target check and the two left-call/call-graph analyses now use the
MRO-aware `grammar_has_user_method_sym` instead of the per-package method table probe.
