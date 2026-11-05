# The parameters of an anonymous `sub` or `method` have their ranges in `.AST`

`sub ($a) { }` and `method ($a, $b) { }` gave their `Parameter` nodes and parameter targets no
origin at all, where named routines and pointy blocks already had one. The anonymous-routine
conversion now places the parser's recorded parameter ranges like the named one does, and a method
literal does so before its invocant parameter joins the list. Closes #16643.
