# `.Str` of a sub or block warns like Rakudo, and `Code.Str` is a method-table row

`&foo.Str` answers the routine's name, as before, but now also emits the warning Rakudo's
`Code.Str` does: `Sub object coerced to string (please use .gist or .raku to do that)` (`Block`
for a block). mutsu used to warn only for a regex. The warning is resumable, so a `CONTROL`
block or `quietly` can silence it.

`Code.Str` is now a method-table row over one routine, `Interpreter::code_str`, which the old
dispatch arms call too (ADR-12523 slice 6). Prefix `~`, string interpolation and a `.^lookup`
method object do not warn yet (#12762).
