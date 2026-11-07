# A user postfix declared looser than a built-in prefix applies to the whole prefix expression

`sub postfix:<@>(\a) is looser(&infix:<->)` makes `-1@` mean `(-1)@`, as in
raku (EERPG's `amount => -1@`). Built-in prefixes (`-`, `+`, `!`, `~`, `?`, ...)
now leave such a postfix for the prefix expression instead of applying it to
the operand first. A postfix with no precedence trait still binds tighter
(`-1§` is `-(1§)`). The same change fixes a deferred postfix being picked up
anyway by the generic operator-character scan when its symbol is non-ASCII
(`§`, `¤`) -- that also affected user-defined prefixes.
