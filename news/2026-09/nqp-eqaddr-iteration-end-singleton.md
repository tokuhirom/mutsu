# `nqp::eqaddr` recognizes `IterationEnd` as a singleton

`nqp::eqaddr(IterationEnd, IterationEnd)` answered `0`, so the canonical
nqp-level iterator loop

```raku
nqp::until(nqp::eqaddr(($x := $it.pull-one), IterationEnd), ...)
```

never terminated. Hash::str, Iter::Able and String::Utils all hung on it
([#9333](https://github.com/tokuhirom/mutsu/issues/9333)).

Root cause: mutsu represents the `IterationEnd` sentinel as the string
`"IterationEnd"`, and every producer (the bareword term's fallback, each
built-in `pull-one`, the iterator-protocol stepper) allocated a fresh `Str`.
`nqp::eqaddr` compares strings by pointer (two separately built strings are
not the same object in rakudo either), so no two sentinels were ever the same
object. `=:=` compares strings by value and so happened to agree with rakudo.

Fix: `Value::iteration_end()` hands out one process-wide allocation, and every
producer, including the bareword `IterationEnd`, uses it. Object identity now
holds wherever the sentinel came from, while a plain `"IterationEnd"` string is
still not the sentinel under `eqaddr`. Pinned by
`t/vm/nqp-eqaddr-iteration-end.t`, which also runs the loop inside a statically
typed (TRIR) routine.
