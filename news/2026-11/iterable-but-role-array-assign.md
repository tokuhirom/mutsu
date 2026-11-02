# `@`-assignment and `for` iterate an Iterable class mixed with a role

`my @b = Q.new but role {...}` and `for Q.new but role {...}` stored or visited the mixed instance
as one item. The `but` result is a `Mixin` whose wrapped class (not the role) declares `iterator`;
list assignment only looked at roles composed on the mixin. It now also honours the wrapped
`Iterable` class's own `iterator`, run with the mixed value as `self`, as rakudo does (#15973).
