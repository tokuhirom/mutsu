# `IterationEnd` is a `Mu` object, and `for` stops at it

`IterationEnd` used to be the string `"IterationEnd"`: `.raku` quoted it,
`.^name` said `Str`, any `Str` spelling that text was treated as the end of an
iterator, and a `for` loop walked straight past a sentinel sitting inside a
list. It is now what rakudo makes it — a unique instance of `Mu` carrying a
reserved instance id, so identity (`=:=`, `nqp::eqaddr`, `===`) compares it by
that id and `::('IterationEnd')` finds the same object.

`.raku`, `.gist` and `.Str` render it by name (as rakudo's `Mu` methods
special-case it), every consumer that used to compare text now asks
`Value::is_iteration_end`, and the eager, live-array and lazy `for` paths stop
at the sentinel (a chunked `-> $a, $b?` loop gets the partial final chunk), so
`.say for ["foo", IterationEnd, "baz"]` prints only `foo`. A `:=` bind to it
binds the object itself, keeping the default `Iterator` role methods'
`$pulled =:= IterationEnd` checks working. (#9809)
