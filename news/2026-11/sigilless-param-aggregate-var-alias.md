# A sigilless parameter keeps its `@`/`%` caller variable through method calls

`C.new(@a, :in-place)` into `multi method new(Iterable \value, ...)` reported `value.VAR` as
`Scalar` the first time a fresh `@a` was passed on, and every time when the invocant was a
parameterized role (`G[* cmp * == More].new(values, :$in-place)`): the call binder dropped the
alias to the caller's variable (the caller's local slot was not in the env yet, and resolving the
role's type arguments consumed the pending argument-source names). BinaryHeap's
`value.VAR !=== value` check then copied the array, so `heapsort({$^b cmp $^a}, @array)` never
sorted the caller's array. BinaryHeap t/03 and t/04 now match raku.
