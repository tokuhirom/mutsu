# `@a[i]:v:delete` leaves a hole, like a plain `:delete`

`my @a = 10, 20; @a[0]:v:delete; say @a[0]:exists` said `True`. Rakudo says
`False`. A plain `@a[0]:delete` was already right (#11320).

The value-adverb companions of `:delete` (`:v`, `:k`, `:p`, `:kv`) wrote the
hole marker into the slot themselves but skipped the bookkeeping that makes it a
hole: dropping the slot from the array's `initialized` set and recording it as
deleted. A type object that is not recorded that way is an element that exists.
That bookkeeping, plus the trailing-hole trim, is now one routine,
`finish_array_slot_delete`, used by the plain `:delete`, the nested-slice
`:delete` and the adverb companions. Its trim now goes through the same root as
the rest of the bookkeeping (a unit lexical slot or the array inside a container
cell), so the companion still shortens the array after a trailing delete.
