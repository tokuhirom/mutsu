# A multi-method call split by the invocant is ambiguous, as on rakudo

Rakudo ranks a multi-method candidate's invocant like any other parameter.
Take two candidates where one has the narrower invocant and the other the
narrower argument type. Neither is narrower, so the call is
`X::Multi::Ambiguous`. Two cases are affected (#18868):

- a `my method` added with `.^add_multi_method`, whose invocant is `Mu`,
  against a class's own `(Late: Cool)` candidate;
- a subclass's `(K: Cool)` against its parent's `(P: Str)`.

mutsu used to run the first-listed candidate in both cases. Three changes
fixed that:

- an added method now carries rakudo's `Mu` invocant;
- the declared-type split check counts the invocant;
- the uncached method-call path now raises an ambiguity the resolution
  recorded, which it used to drop. Both call paths share the new
  `take_dispatch_ambiguity` helper.
