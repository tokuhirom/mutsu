# `.reduce(&[~])` appends in place instead of copying every step

`@a.reduce(&[~])` was quadratic: it took 0.92 s on 80,000 elements (0.08 s on
20,000, so each doubling cost about 4x). The reduce loop called the builtin
routine with a *clone* of its accumulator, because a user block has to keep the
accumulator through a `next` or `last`. The routine-call path
(`call_infix_routine`) then cloned it again before concatenating. With the
accumulated `Str` always held twice, the in-place append (#9141) could never
fire, and every step copied the whole string.

When the callable is the builtin `infix:<~>` and no user `infix:<~>` candidate
is in scope, the fold now goes through `Interpreter::infix_concat` with the
accumulator moved in. The builtin routine cannot raise `next` or `last`, so
nothing needs a second copy. `infix_concat` is the body of the `~` opcode,
split out so the opcode and the fold share one implementation: junction
threading, the `.Stringy` coercion of each operand, Blob rules, then
`concat_values`. The same fold is 0.012 s at 80,000 elements and linear
(`scripts/array-complexity-check.sh`'s new `.reduce(&[~])` case: ratio 1.95).

`[~] @a` itself was already linear by now. Its reduction opcode moves the
accumulator into each step, and after #9272 a shared operand joins strands
instead of copying. Its `Rakudo: O(t)` suffix is dropped.

Pinned by `t/types/string/reduce-concat-fold.t`. The `.unique` / `.repeated`
half of #9161 is unrelated to strings and is still open.
