# A method call short of its required positionals is refused after a successful call

`P.new.m(3)` followed by `P.new.m()` (with `method m($x)`) ran the second call instead of
raising "Too few positionals". The fast method cache that the first successful call fills
only bounded the argument count from above (`args.len() <= positional_count`). Its entry now
records the required positional count and a call below it takes the full path, which raises
the arity error; submethods and role methods share the cache (#14589).
