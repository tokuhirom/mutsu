# A role composition remembers the `+{...}` part of its type name

`what_type_name` renders a role-mixed value or type object as
`<base>+{R1}+{R2}`; building the suffix walks every marker of the composition,
sorts the roles by application order and formats each one. A `Type but Role`
type object (Monad's `parameterized(M, T).^pun but Monad::Maybe::Some`) is
named that way on every `=:=` against it, every smartmatch and every
`.^name`, so the same suffix was rebuilt several times per operation.

`MixinOverrides` now memoizes the suffix for the base name it was last
rendered against. The suffix is a function of the composition's markers
alone, and every `&mut` accessor of the marker map clears the memo.

`Monad::Maybe.some(1)`: 622k -> 605k instructions per call (callgrind,
profiling build, 200 calls, warm). The remaining renders are of the fresh
instance mixin each `.new` builds, whose name is asked once. Refs #17331.
