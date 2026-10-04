# `.grep()` without a matcher dies like rakudo

`(1, 2, 3).grep()` returned the list unchanged, because the method treated a
missing matcher as "keep everything". Every rakudo `grep` candidate takes a
matcher (`($: Bool:D $t, *%_)`, `($: Mu $t, *%_)`), so the call resolves none
of them. mutsu now raises the same `X::Multi::NoMatch`, naming the invocant
type and any adverbs (`Cannot resolve caller grep(List:D: :k); none of these
signatures matches:` followed by the two candidates). Closes #11630.
