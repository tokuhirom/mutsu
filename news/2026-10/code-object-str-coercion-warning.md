# Prefix ~, interpolation and Method.Str warn for a code object

Every stringification of a code object now goes through `Interpreter::code_str`:
prefix `~`, infix `~`, string interpolation and `Method`/`Submethod` `.Str` (a
`.^lookup` result) warn `<Type> object coerced to string (please use .gist or .raku
to do that)` and answer the routine's name, as Rakudo does. Before, only an
explicit `.Str` on a `Sub` warned.
