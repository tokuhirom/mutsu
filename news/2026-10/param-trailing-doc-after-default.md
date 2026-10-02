# A `#=` after a defaulted parameter documents the routine, as in Rakudo

In `sub f($a = 1 #= doc\n) {}`, the trailing declarator comment used to
document `$a`. Rakudo gives it to the routine (`&f.WHY` is `doc`). A
parameter keeps a trailing `#=` only when the comment comes right after its
variable (`$a #= doc`, `Int $a? #= doc`, `:$a! #= doc`) or after the `,` that
ends it. After a default, a `where` clause or a trait with no comma before the
comment, the routine gets it. After a sigilless `\a`, the routine gets it
with or without a comma; mutsu used to drop that comment entirely (#10953).

The parameter parser returns its input past the trailing whitespace, so a
trailing comment sat inside the parameter's recorded extent. `attach_param`
now cuts the extent where such a comment starts. Only comments that `ws`
actually recorded count, so a `#` inside a string default is not taken for
one.
