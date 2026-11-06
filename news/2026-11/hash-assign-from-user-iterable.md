# Assigning a user `Iterable` to a `%` variable iterates it

`my %h = $obj` (and `is Map` / `is Hash`) with a class that does `Iterable`
through its own `iterator` stored the object as one stringified key, while
`my @a = $obj` iterated it. The hash coercion now drives the iterator (unless it
is lazy). Found by ParaSeq 0.2.7 `t/01-basic.rakutest` test 20 (#16475).

The same ParaSeq run needed two `ConcBlockingQueue` ops: `nqp::atpos($q, 0)` now
peeks the head (null when empty; MoarVM refuses any other index) and
`nqp::unshift($q, $v)` puts an item at the head.
