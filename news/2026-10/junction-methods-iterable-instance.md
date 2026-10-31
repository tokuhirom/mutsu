# `.all` / `.any` / `.one` / `.none` on an `Iterable` object

`class It does Iterable { method iterator { (4, 5).iterator } }; It.new.all` was
`all(It.new)` in mutsu and `all(4, 5)` in rakudo. `Any.all` reads the object's
`.iterator` (the `.list` of an `Iterable`-only object is the object itself, which
is why the previous `Positional` fix, going through `.list`, did not cover it).
An `Iterable` instance is now drained through its iterator; a `Positional`-only
one still goes through `.list`, and a plain object stays a single item.
