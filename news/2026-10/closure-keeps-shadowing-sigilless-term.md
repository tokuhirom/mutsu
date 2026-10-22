# A closure keeps a shadowing uppercase sigilless term

`my \U = 'inner'` in a block or routine, read by a closure that is called after
the block exited while an outer `\U` exists, gave the outer value: the
capture analysis treated an uppercase term key as a type name and left it to the
caller's env chain. A term key is never a type name, so it is captured like a
lowercase one (`env::is_user_lexical_or_term_key`). This also fixes a block
`use` whose imported type shadows an outer term of the same name (#14435).
