# `subset ... where Class|Interface` resolves imported types in the declaring module

A `where` predicate that is a value (not a code block) and names types the declaring module imported, such as `my subset Unit where Class|Interface`, was evaluated in the checking caller's scope. If the caller had not imported the same types, the bare names fell back to strings and the check failed. Such predicates are now closed over their declaration scope, like block predicates that name types already were. Found with `Java::Generate` (`t/09-comp-unit.t` now passes).
