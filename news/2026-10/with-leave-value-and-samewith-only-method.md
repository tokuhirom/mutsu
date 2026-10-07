# `with`/`given` bodies with LEAVE keep their value; `samewith` in a lone method takes `self`

Found by running the UpRooted distribution (`t/43-writer-postgresqlfile.rakutest`).

A `with`/`given` body carrying a `LEAVE` phaser in tail position used to evaluate to `Nil`, because
the phaser block scope discarded the body value (DBDish::Pg::Native's `quote` ends in such a
`with ... { LEAVE { ... }; nativecast(Str, $_) }`). It now pushes the value like every other
`given`, so a non-final one is popped by the statement loop and a final one is the block value.

`samewith` inside a non-multi method re-calls the routine like a sub in Rakudo: the invocant is the
first argument (`samewith(self, $x)`). Multi methods keep the implicit invocant. mutsu used to
prepend the invocant in both cases, so UpRooted's `samewith( self, $_, Any:U )` got one argument too
many.
