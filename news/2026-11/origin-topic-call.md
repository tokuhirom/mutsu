# A topic call (`.name`) carries origins on its call, name and arguments

`.abs`, `.say` and `.substr(1, 2)` (a `Term::TopicCall`), as an argument, a
statement of a block or inside an interpolated `{...}`, had a range only on the
topic call itself; its `Call::Method`, `Name` and `ArgList` had none. They now
get the ranges rakudo gives them, through the same placement as the postfix
`.name(...)` form (`call_origins::place_call`, which now takes an optional
field name so it can place a call held positionally). Part of #15780.
