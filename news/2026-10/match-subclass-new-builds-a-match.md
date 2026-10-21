# `Match` subclass `.new` builds a Match

`class M is Match { }; M.new(:orig('hello'), :from(1), :pos(3))` produced a
plain instance with none of `Match`'s methods (`No such method 'from'`). A
subclass without its own `new` now builds the same value as `Match.new` and
marks it as a match, so `.from`, `.to`, `.Str`, `.orig` and the rest answer as
in Rakudo (#14338).
