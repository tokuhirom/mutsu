# hide-methods now passes: protect-block subs, `.^can` method objects, add_method lexical types

Three interpreter gaps surfaced by the `hide-methods` distribution (27/27 assertions of
`t/01-basic.rakutest` now pass, up from 20):

- A `sub` declared inside an inline `Lock.protect` block lost its body once the block returned
  (calling it answered `Nil`), because the cached block carried an empty nested-function table
  instead of the compiled code's own.
- `.^can` now answers the same `Method` object `.^find_method` / `.^lookup` do, so a role mixed
  into a method (`$m does R`) is visible through it.
- Code handed to `^add_method` from a module keeps the module's `my class` / `my role` in scope
  when it runs as a method of an unrelated class.
