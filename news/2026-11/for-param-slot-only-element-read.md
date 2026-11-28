# A `for` body that reads `@a[$i]` binds its parameter in the slot only

A `for ... -> $i` loop whose body cannot observe how `$i` is bound skips the
per-iteration env binding of the parameter (`ForLoopSpec::param_slot_only`), and
the same proof lets an implicit topic live in a slot of its own. The proof's
allow-list admitted hash element reads (`%h{$k}`) but not array element reads,
so the most common loop shape of all, `for ^N -> $i { $s += @a[$i] }`, paid a
`rebind_sym` of `$i` into the env on every iteration -- about 110 instructions,
in `bench-threads`' reader loop among others (#19406).

An rvalue positional read of an `@` variable is now admitted: it passes the
subscript's value on and writes nothing. Element writes (`@a[$i] = v`,
`@a[$i]++`), slices, Whatever subscripts and `$a[$i]` on a scalar stay refused.
`t/collections/for-topic-element-read.t` pins the value, the untouched outer
variable of the same name, the readonly parameter, per-iteration closures and a
`start` block's read.
