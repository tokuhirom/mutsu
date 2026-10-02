# A `Str` subclass renders its payload in string contexts

An instance of a user subclass of `Str` that declared its own `Str` method
(`class MyStr is Str { method Str { "strr" } }`) used that method everywhere a
string was needed: interpolation, infix `~`, `.Stringy`, `join` and list
stringification all printed `strr` for `MyStr.new(value => "q")`. Rakudo
treats the instance as the `Str` it already is. `Str.Stringy` returns `self`,
so interpolation renders the payload `q`. The native string operators (infix
`~`, `eq`, `join`) read the payload even when the subclass declares its own
`Stringy`. Only prefix `~` and `.Str` call the subclass's `Str`.

mutsu now follows the same split (#11026), the mirror image of the
`Int`/`Num` subclass fix in #10992.
