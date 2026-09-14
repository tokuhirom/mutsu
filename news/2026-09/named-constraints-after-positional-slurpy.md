Named parameters after a positional slurpy now match Rakudo's binding
semantics: nominal and callable-return constraints are not enforced for those
parameters, while an explicit `where` constraint remains active. This lets
List::Allmax pass its `Callable :&by` callback test.

Pinned by `t/routines/signature/named-constraint-after-variadic-signature.t`.
