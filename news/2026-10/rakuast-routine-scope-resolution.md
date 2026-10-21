RakuAST routine nodes now answer `.scope` with Rakudo's current defaults:
`anon` for nameless routines, `my` for named subs, and `has` for named
methods and submethods. Explicit `my` and `our` submethod scopes remain
observable while their constructor rendering omits the scope, as Rakudo does.
The same nodes keep their scope when evaluated through the RakuAST frontend.
