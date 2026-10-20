# A `where` on `*@slurpy` no longer sees the call's named arguments

`multi red-do(*@blocks where .all ~~ Callable, Str :$with = "default")` failed to match
`red-do({;}, :with<x>)` because the named argument's Pair was part of the topic `.all` checked.
Named arguments are now left out of the positional slurpy during candidate matching, as they are when
it is bound. Found through the Red distribution.
