# A `my \k = EXPR` term keeps its readonly state in its slot

A sigilless term declared from a non-variable expression (`my \key = M.WHICH ~
'|'`) is bound to a value and readonly for its whole life. mutsu recorded that
as an env key written twice per declaration, and in a routine called on the
light path that write was the first one into the frame's env overlay, so every
call allocated an overlay, copied it and merged it back: the declaration cost
~17.5k instructions against ~6.8k for `my $key`.

The verdict is now a compile-time flag of the term's slot (ADR-17376). A term
nothing outside its routine can name keeps no env key and takes the env-free
declaration lane; assignments, `++`/`--`, compound assignment and `.=` ask the
slot, and passing the term to a callee hands over its value rather than a
variable the callee could alias. `my \key = "a"` in a routine now costs ~7.5k
instructions. As a side effect, passing such a term to an `is rw` parameter and
writing through `:k(k)` are refused as in rakudo, where mutsu accepted both.
