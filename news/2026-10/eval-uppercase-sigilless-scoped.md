# An EVAL's uppercase `my \Name` no longer outlives the EVAL

`EVAL q[my \Zork := 42]` left `Zork` visible to later EVALs because the
unit's lexical cleanup only drops lowercase names (an uppercase name could be a
type). The snippet's own sigilless declarations are now collected with the
other names it declares, so their term key and readonly marker are restored to
the caller's state when the EVAL ends (#14227).
