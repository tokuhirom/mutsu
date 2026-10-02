# A `Rat` subclass is constructed positionally and renders like rakudo's

`class R is Rat {}; R.new(1, 2)` died with "Default constructor for 'R' only
takes named arguments". `Int` and `Num` subclasses already kept their number
in a reserved payload attribute. A `Rat` subclass now does the same
(`__mutsu_rat_value`, built by the native `Rat.new`, so the fraction is
reduced). Arithmetic, comparison and the inherited `Rat` methods (`.numerator`,
`.nude`, `.Rat`, ...) answer on it (#11025).

Rendering follows rakudo for all three numeric subclasses. `.gist` goes
through a user `Str` (`Int.gist`, `Num.gist` and `Rational.gist` are
`self.Str`), so `class MyRat is Rat { method Str { "ratt" } }` prints `ratt`
under `say`. `.raku` of an `Int`/`Num` subclass with a user `Str` is that
string, with `e0` appended for a `Num`. A `Rat` subclass renders as `<3/4>`,
or `2.0` for an integral value, never in the decimal form a plain `Rat` uses.
