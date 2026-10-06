# `Parameter.gist` prints the parameter's `.raku`

`Parameter.gist` (and `say $param`) printed `Parameter.new` for every parameter. It now answers the
same text as `.raku`, as Rakudo does: `Int $`, `Str:D $y`, `:$x`, `K:D $:`.
